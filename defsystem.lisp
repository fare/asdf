;;;; -------------------------------------------------------------------------
;;;; Defsystem

(defpackage :asdf/defsystem
  (:use :common-lisp :asdf/utility :asdf/pathname
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component
   :asdf/lisp-action :asdf/operate
   :asdf/backward-internals)
  #+gcl<2.7 (:shadowing-import-from :asdf/implementation #:type-of)
  (:export
   #:defsystem #:do-defsystem #:parse-component-form
   #:*default-component-class*
   ))
(in-package :asdf/defsystem)

;;; Pathname

(defun* determine-system-pathname (pathname)
  ;; The defsystem macro calls us to determine
  ;; the pathname of a system as follows:
  ;; 1. the one supplied,
  ;; 2. derived from *load-pathname* via load-pathname
  ;; 3. taken from the *default-pathname-defaults* via default-directory
  (let* ((file-pathname (load-pathname))
         (directory-pathname (and file-pathname (pathname-directory-pathname file-pathname))))
    (or (and pathname (subpathname directory-pathname pathname :type :directory))
        directory-pathname
        (default-directory))))

;;; Component class

(defvar *default-component-class* 'cl-source-file)

(defun* class-for-type (parent type)
  (or (loop :for symbol :in (list
                             type
                             (find-symbol* type *package* nil)
                             (find-symbol* type :asdf/interface nil))
        :for class = (and symbol (find-class* symbol nil))
        :when (and class
                   (#-cormanlisp subtypep #+cormanlisp cl::subclassp
                                 class (find-class* 'component)))
        :return class)
      (and (eq type :file)
           (find-class*
            (or (loop :for p = parent :then (component-parent p) :while p
                      :thereis (module-default-component-class p))
                *default-component-class*) nil))
      (sysdef-error "don't recognize component type ~A" type)))

;;; Check inputs

(define-condition duplicate-names (system-definition-error)
  ((name :initarg :name :reader duplicate-names-name))
  (:report (lambda (c s)
             (format s (compatfmt "~@<Error while defining system: multiple components are given same name ~A~@:>")
                     (duplicate-names-name c)))))

(defun* sysdef-error-component (msg type name value)
  (sysdef-error (strcat msg (compatfmt "~&~@<The value specified for ~(~A~) ~A is ~S~@:>"))
                type name value))

(defun* check-component-input (type name weakly-depends-on
                              depends-on components in-order-to)
  "A partial test of the values of a component."
  (unless (listp depends-on)
    (sysdef-error-component ":depends-on must be a list."
                            type name depends-on))
  (unless (listp weakly-depends-on)
    (sysdef-error-component ":weakly-depends-on must be a list."
                            type name weakly-depends-on))
  (unless (listp components)
    (sysdef-error-component ":components must be NIL or a list of components."
                            type name components))
  (unless (and (listp in-order-to) (listp (car in-order-to)))
    (sysdef-error-component ":in-order-to must be NIL or a list of components."
                            type name in-order-to)))

;;; Main parsing function

(defun* parse-component-form (parent options &key previous-serial-component)
  (destructuring-bind
        (type name &rest rest &key
              ;; the following list of keywords is reproduced below in the
              ;; remove-keys form.  important to keep them in sync
              components pathname
              perform explain output-files operation-done-p
              weakly-depends-on depends-on serial in-order-to
              do-first if-component-dep-fails
              (version nil versionp)
              ;; list ends
              &allow-other-keys) options
    (declare (ignorable perform explain output-files operation-done-p))
    (check-component-input type name weakly-depends-on depends-on components in-order-to)
    (when (and parent
               (find-component parent name)
               (not ;; ignore the same object when rereading the defsystem
                (typep (find-component parent name)
                       (class-for-type parent type))))
      (error 'duplicate-names :name name))
    (when versionp
      (unless (parse-version version nil)
        (warn (compatfmt "~@<Invalid version ~S for component ~S~@[ of ~S~]~@:>")
              version name parent)))
    (when do-first (error "DO-FIRST is not supported anymore since ASDF 2.27"))
    (let* ((args `(:name ,(coerce-name name)
                   :pathname ,pathname
                   ,@(when parent `(:parent ,parent))
                   ,@(remove-keys
                      '(components pathname if-component-dep-fails
                        perform explain output-files operation-done-p
                        weakly-depends-on depends-on serial in-order-to)
                      rest)))
           (ret (find-component parent name)))
      (when weakly-depends-on
        (appendf depends-on (remove-if (complement #'(lambda (x) (find-system x nil))) weakly-depends-on)))
      (when previous-serial-component
        (push previous-serial-component depends-on))
      (if ret ; preserve identity
          (apply 'reinitialize-instance ret args)
          (setf ret (apply 'make-instance (class-for-type parent type) args)))
      (component-pathname ret) ; eagerly compute the absolute pathname
      (when (typep ret 'parent-component)
        (setf (component-children ret)
              (loop
                :with previous-component = nil
                :for c-form :in components
                :for c = (parse-component-form ret c-form
                                               :previous-serial-component previous-component)
                :for name = (component-name c)
                :collect c
                :when serial :do (setf previous-component name)))
        (compute-children-by-name ret))
      (setf (component-sibling-dependencies ret) depends-on) ;; Used by POIU. ASDF3: rename to component-depends-on
      (setf (component-in-order-to ret) in-order-to)
      (%refresh-component-inline-methods ret rest)
      (when if-component-dep-fails (%resolve-if-component-dep-fails if-component-dep-fails ret))
      ret)))

(defun* do-defsystem (name &rest options
                           &key pathname (class 'system)
                           defsystem-depends-on &allow-other-keys)
  ;; The system must be registered before we parse the body,
  ;; otherwise we recur when trying to find an existing system
  ;; of the same name to reuse options (e.g. pathname) from.
  ;; To avoid infinite recursion in cases where you defsystem a system
  ;; that is registered to a different location to find-system,
  ;; we also need to remember it in a special variable *systems-being-defined*.
  (with-system-definitions ()
    (let* ((name (coerce-name name))
           (load-pathname (load-pathname))
           (registered (system-registered-p name))
           (registered! (if registered
                            (rplaca registered (safe-file-write-date load-pathname))
                            (register-system (make-instance 'system :name name :source-file load-pathname))))
           (system (reset-system (cdr registered!)
                                :name name :source-file load-pathname))
           (component-options (remove-keys '(:class) options)))
      (setf (gethash name *systems-being-defined*) system)
      (apply 'load-systems defsystem-depends-on)
      ;; We change-class (when necessary) AFTER we load the defsystem-dep's
      ;; since the class might be defined as part of those.
      (let ((class (class-for-type nil class)))
        (unless (eq (type-of system) class)
          (change-class system class)))
      (parse-component-form
       nil (list*
            :module name
            :pathname (determine-system-pathname pathname)
            component-options)))))

(defmacro defsystem (name &body options)
  `(apply 'do-defsystem ',name ',options))
