;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(asdf/package:define-package :asdf/operate
  (:recycle :asdf/operate :asdf)
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/system :asdf/operation :asdf/action
   :asdf/find-system :asdf/find-component :asdf/lisp-action :asdf/plan)
  (:export
   #:operate #:oos
   #:*systems-being-operated*
   #:build-system
   #:load-system #:load-systems #:compile-system #:test-system #:require-system
   #:*load-system-operation* #:module-provide-asdf
   #:component-loaded-p #:already-loaded-systems))
(in-package :asdf/operate)

(with-upgradability ()
  (defgeneric* (operate) (operation component &key &allow-other-keys)
    (:documentation
     "Operate does three things:

1. It creates an instance of OPERATION-CLASS using any keyword parameters as initargs.
2. It finds the  asdf-system specified by SYSTEM (possibly loading it from disk).
3. It then calls TRAVERSE with the operation and system as arguments

The traverse operation is wrapped in WITH-COMPILATION-UNIT and error handling code.
If a VERSION argument is supplied, then operate also ensures that the system found
satisfies it using the VERSION-SATISFIES method.

Note that dependencies may cause the operation to invoke other operations on the system
or its components: the new operations will be created with the same initargs as the original one.

The :FORCE or :FORCE-NOT argument to OPERATE can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  (SYSTEM1 SYSTEM2 ... SYSTEMN) to force systems named in a given list
:FORCE has precedence over :FORCE-NOT; builtin systems cannot be forced."))

  (define-convenience-action-methods
      operate (operation component &key)
      ;; I'd like to at least remove-plist-keys :force :force-not :verbose,
      ;; but swank.asd relies on :force (!).
      :operation-initargs t ;; backward-compatibility with ASDF1. Yuck.
      :if-no-component (error 'missing-component :requires component))

  (defvar *systems-being-operated* nil
    "A boolean indicating that some systems are being operated on")

  (defmethod operate :around (operation component &rest keys
                              &key verbose
                                (on-warnings *compile-file-warnings-behaviour*)
                                (on-failure *compile-file-failure-behaviour*) &allow-other-keys)
    (declare (ignorable operation component))
    (let* ((systems-being-operated *systems-being-operated*)
           (*systems-being-operated* (or systems-being-operated (make-hash-table :test 'equal)))
           (operation-name (reify-symbol (etypecase operation
                                           (operation (type-of operation))
                                           (symbol operation))))
           (component-path (typecase component
                             (component (component-find-path component))
                             (t component))))
      ;; Before we operate on any system, make sure ASDF is up-to-date,
      ;; for if an upgrade is ever attempted at any later time, there may be BIG trouble.
      (unless systems-being-operated
        (when (upgrade-asdf)
          ;; If we were upgraded, restart OPERATE the hardest of ways, for
          ;; its function may have been redefined, its symbol uninterned, its package deleted.
          (return-from operate
            (apply (find-symbol* 'operate :asdf)
                   (unreify-symbol operation-name)
                   component-path keys))))
      ;; Setup proper bindings around any operate call.
      (with-system-definitions ()
        (let* ((*verbose-out* (and verbose *standard-output*))
               (*compile-file-warnings-behaviour* on-warnings)
               (*compile-file-failure-behaviour* on-failure))
          (call-next-method)))))

  (defmethod operate :before ((operation operation) (component component)
                              &key version &allow-other-keys)
    (let ((system (component-system component)))
      (setf (gethash (coerce-name system) *systems-being-operated*) system))
    (unless (version-satisfies component version)
      (error 'missing-component-of-version :requires component :version version)))

  (defmethod operate ((operation operation) (component component)
                      &rest keys &key &allow-other-keys)
    (let ((plan (apply 'traverse operation component keys)))
      (perform-plan plan)
      (values operation plan)))

  (defun oos (operation component &rest args &key &allow-other-keys)
    (apply 'operate operation component args))

  (setf (documentation 'oos 'function)
        (format nil "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
                (documentation 'operate 'function))))


;;;; Common operations
(with-upgradability ()
  (defvar *load-system-operation* 'load-op
    "Operation used by ASDF:LOAD-SYSTEM. By default, ASDF:LOAD-OP.
You may override it with e.g. ASDF:LOAD-FASL-OP from asdf-bundle,
or ASDF:LOAD-SOURCE-OP if your fasl loading is somehow broken.

This may change in the future as we will implement component-based strategy
for how to load or compile stuff")

  (defun build-system (system &rest keys)
    "Shorthand for `(operate 'asdf:build-op system)`."
    (apply 'operate 'build-op system keys)
    t)

  (defun load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate *load-system-operation* system keys)
    t)

  (defun load-systems (&rest systems)
    "Loading multiple systems at once."
    (map () 'load-system systems))

  (defun compile-system (system &rest args &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(asdf:operate 'asdf:compile-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'compile-op system args)
    t)

  (defun test-system (system &rest args &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(asdf:operate 'asdf:test-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'test-op system args)
    t))


;;;; Define require-system, to be hooked into CL:REQUIRE when possible,
;; i.e. for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL
(with-upgradability ()
  (defun component-loaded-p (c)
    (action-already-done-p nil (make-instance 'load-op) (find-component c ())))

  (defun already-loaded-systems ()
    (remove-if-not 'component-loaded-p (registered-systems)))

  (defun require-system (s &rest keys &key &allow-other-keys)
    (apply 'load-system s :force-not (already-loaded-systems) keys))

  (defvar *modules-being-required* nil)

  (defclass require-system (system)
    ((module :initarg :module :initform nil :accessor required-module)))

  (defmethod perform ((o compile-op) (c require-system))
    (declare (ignorable o c))
    nil)

  (defmethod perform ((o load-op) (s require-system))
    (declare (ignorable o))
    (let* ((module (or (required-module s) (coerce-name s)))
           (*modules-being-required* (cons module *modules-being-required*)))
      (assert (null (component-children s)))
      (require module)))

  (defmethod resolve-dependency-combination (component (combinator (eql :require)) arguments)
    (declare (ignorable component combinator))
    (unless (length=n-p arguments 1)
      (error (compatfmt "~@<Bad dependency ~S for ~S. ~S takes only one argument~@:>")
             (cons combinator arguments) component combinator))
    (let* ((module (car arguments))
           (name (string-downcase module)))
      (assert module)
      (make-instance 'require-system :name name)))

  (defun module-provide-asdf (name)
    (let ((module (string-downcase name)))
      (unless (member module *modules-being-required* :test 'equal)
        (let ((*modules-being-required* (cons module *modules-being-required*))
              #+sbcl (sb-impl::*requiring* (remove module sb-impl::*requiring* :test 'equal)))
          (handler-bind
              ((style-warning #'muffle-warning)
               (missing-component (constantly nil))
               (error #'(lambda (e)
                          (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                                  name e))))
            (let ((*verbose-out* (make-broadcast-stream)))
              (let ((system (find-system module nil)))
                (when system
                  (require-system system :verbose nil)
                  t)))))))))


;;;; Some upgrade magic
(with-upgradability ()
  (defun restart-upgraded-asdf ()
    ;; If we're in the middle of something, restart it.
    (when *systems-being-defined*
      (let ((l (loop :for name :being :the :hash-keys :of *systems-being-defined* :collect name)))
        (clrhash *systems-being-defined*)
        (dolist (s l) (find-system s nil)))))

  (register-hook-function '*post-upgrade-restart-hook* 'restart-upgraded-asdf))


