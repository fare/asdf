;;;; -------------------------------------------------------------------------
;;;; Systems

(uiop/package:define-package :asdf/system
  (:recycle :asdf :asdf/system)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/component)
  (:export
   #:system #:proto-system
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:reset-system
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:system-defsystem-depends-on #:system-depends-on #:system-weakly-depends-on
   #:compute-system-variables #:configure-system-variables #:configure-system-variable
   #:with-updated-system-variables
   #:use-system-variables #:initialize-system-variables #:update-system-variables
   #:system-variable-specs #:system-variable-names #:system-variable-values #:system-variable-initializers
   #:component-build-pathname #:build-pathname
   #:component-entry-point #:entry-point
   #:homepage #:system-homepage
   #:bug-tracker #:system-bug-tracker
   #:mailto #:system-mailto
   #:long-name #:system-long-name
   #:source-control #:system-source-control
   #:find-system #:builtin-system-p)) ;; forward-reference, defined in find-system
(in-package :asdf/system)

(with-upgradability ()
  (defgeneric* (find-system) (system &optional error-p))
  (defgeneric* (system-source-file :supersede #-clisp t #+clisp nil) (system)
    (:documentation "Return the source file in which system is defined."))
  (defgeneric component-build-pathname (component))

  (defgeneric component-entry-point (component))
  (defmethod component-entry-point ((c component))
    nil))

;;;; The system class
(with-upgradability ()
  (defclass proto-system () ; slots to keep when resetting a system
    ;; To preserve identity for all objects, we'd need keep the components slots
    ;; but also to modify parse-component-form to reset the recycled objects.
    ((name) (source-file) #|(children) (children-by-names)|#))

  (defclass system (module proto-system)
    ;; Backward-compatibility: inherit from module. ASDF4: only inherit from parent-component.
    (;; {,long-}description is now inherited from component, but we add the legacy accessors
     (description :accessor system-description)
     (long-description :accessor system-long-description)
     (author :accessor system-author :initarg :author :initform nil)
     (maintainer :accessor system-maintainer :initarg :maintainer :initform nil)
     (licence :accessor system-licence :initarg :licence
              :accessor system-license :initarg :license :initform nil)
     (homepage :accessor system-homepage :initarg :homepage :initform nil)
     (bug-tracker :accessor system-bug-tracker :initarg :bug-tracker :initform nil)
     (mailto :accessor system-mailto :initarg :mailto :initform nil)
     (long-name :accessor system-long-name :initarg :long-name :initform nil)
     ;; Conventions for this slot aren't clear yet as of ASDF 2.27, but whenever they are, they will be enforced.
     ;; I'm introducing the slot before the conventions are set for maximum compatibility.
     (source-control :accessor system-source-control :initarg :source-control :initform nil)
     (builtin-system-p :accessor builtin-system-p :initform nil :initarg :builtin-system-p)
     (build-pathname
      :initform nil :initarg :build-pathname :accessor component-build-pathname)
     (entry-point
      :initform nil :initarg :entry-point :accessor component-entry-point)
     (source-file :initform nil :initarg :source-file :accessor system-source-file)
     (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on
                           :initform nil)
     ;; these two are specially set in parse-component-form, so have no :INITARGs.
     (depends-on :reader system-depends-on :initform nil)
     (weakly-depends-on :reader system-weakly-depends-on :initform nil)
     ;; System variables
     (variable-specs :initform nil :initarg :variables :reader system-variable-specs)
     (variable-names :accessor system-variable-names)
     (variable-values :accessor system-variable-values)
     (variable-initializers :accessor system-variable-initializers)))

  (defgeneric configure-system-variable (system variable &key))
  (defmethod configure-system-variable ((system system) variable &key (initializer nil initp))
    ;; You should configure system variables only during the call to compute-system-variables.
    ;; During configuration, the initializer hash-table ensures each variable appears only once,
    ;; modulo the fact that you can designate a variable using multiple names, in which case you lose.
    ;; The heuristics should be simple, though: if it's a CL or ASDF variable, just use it;
    ;; otherwise, use a string that has : if and only if the variable is exported,
    ;; which it should be if you're not the author of the system.
    ;; Beware that initializers will be called in the order the variables were declared,
    ;; which, if you override a variable's initializer,
    ;; will be earlier than other variables you're initializing.
    (let ((value (variable-value variable :package :asdf :when-undefined nil))
          (previousp (nth-value 1 (gethash variable (system-variable-initializers system)))))
      (unless previousp
        (push variable (system-variable-names system))
        (push value (system-variable-values system)))
      (when (or initp (not previousp))
        (setf (gethash variable (system-variable-initializers system))
              (if initp initializer (constantly value)))))
    nil)

  (defgeneric configure-system-variables (system variable-specs))
  (defmethod configure-system-variables ((system system) variable-specs)
    (dolist (spec variable-specs)
      (multiple-value-bind (variable keys)
          (if (consp spec)
              (values (first spec) (when (consp (cdr spec)) (list :initializer (second spec))))
              (values spec nil))
        (apply 'configure-system-variable system variable keys))))

  (defgeneric use-system-variables (system))
  (defmethod use-system-variables ((system system))
    (loop :for variable :in (system-variable-names system)
          :for value :in (system-variable-values system) :do
            (setf (variable-value variable :package :asdf :when-undefined nil) value)))

  (defgeneric update-system-variables (system))
  (defmethod update-system-variables ((system system))
    (setf (system-variable-values system)
          (loop :for variable :in (system-variable-names system)
                :collect (variable-value variable :package :asdf :when-undefined nil))))

  (defgeneric initialize-system-variables (system))
  (defmethod initialize-system-variables ((system system))
    (loop :for variable :in (system-variable-names system)
          :for initializer = (gethash variable (system-variable-initializers system))
          :for var = (ensure-variable variable :package :asdf :when-undefined nil)
          :when var :do (setf (symbol-value var) (call-function initializer))))

  (defmacro with-updated-system-variables ((component) &body body)
    `(call-with-updated-system-variables ,component #'(lambda () ,@body)))

  (defun call-with-updated-system-variables (component thunk)
    (loop :with system = (component-system component)
          :for name :in (system-variable-names system)
          :for value :in (system-variable-values system)
          :for variable = (ensure-variable name :package :asdf :when-undefined nil)
          :when variable
            :collect variable :into vars
            :and :collect value :into vals
          :finally
             (progv vars vals
               (prog1
                   (funcall thunk)
                 (update-system-variables system)))))

  (defgeneric compute-system-variables (system))
  (defmethod compute-system-variables ((system system))
    (configure-system-variables system (system-variable-specs system))
    ;; TODO: insert an out-of-band system configuration facility <here>, or in an :after method.
    nil)

  (defmethod shared-initialize :after ((system system) slot-names &key)
    (declare (ignore slot-names))
    (setf (system-variable-names system) nil
          (system-variable-values system) nil
          (system-variable-initializers system) (make-hash-table :test 'equal))
    (compute-system-variables system)
    (setf (system-variable-names system) (reverse (system-variable-names system))
          (system-variable-values system) (reverse (system-variable-values system))))

  (defun reset-system (system &rest keys &key &allow-other-keys)
    (change-class (change-class system 'proto-system) 'system)
    (apply 'reinitialize-instance system keys)))


;;;; Pathnames

(with-upgradability ()
  (defmethod system-source-file ((system-name string))
    (system-source-file (find-system system-name)))
  (defmethod system-source-file ((system-name symbol))
    (system-source-file (find-system system-name)))

  (defun system-source-directory (system-designator)
    "Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located."
    (pathname-directory-pathname (system-source-file system-designator)))

  (defun (system-relative-pathname) (system name &key type)
    (subpathname (system-source-directory system) name :type type))

  (defmethod component-pathname ((system system))
    (let ((pathname (or (call-next-method) (system-source-directory system))))
      (unless (and (slot-boundp system 'relative-pathname) ;; backward-compatibility with ASDF1-age
                   (slot-value system 'relative-pathname)) ;; systems that directly access this slot.
        (setf (slot-value system 'relative-pathname) pathname))
      pathname))

  (defmethod component-relative-pathname ((system system))
    (parse-unix-namestring
     (and (slot-boundp system 'relative-pathname)
          (slot-value system 'relative-pathname))
     :want-relative t
     :type :directory
     :ensure-absolute t
     :defaults (system-source-directory system)))

  (defmethod component-parent-pathname ((system system))
    (system-source-directory system))

  (defmethod component-build-pathname ((c component))
    nil))

