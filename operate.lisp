;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(uiop/package:define-package :asdf/operate
  (:recycle :asdf/operate :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/operation :asdf/action
   :asdf/find-system :asdf/find-component :asdf/lisp-action :asdf/plan)
  (:export
   #:operate #:oos
   #:build-op #:make
   #:load-system #:load-systems #:load-systems*
   #:compile-system #:test-system #:require-system
   #:module-provide-asdf
   #:component-loaded-p #:already-loaded-systems))
(in-package :asdf/operate)

(with-upgradability ()
  (defgeneric operate (operation component &key &allow-other-keys)
    (:documentation
     "Operate does mainly four things for the user:

1. Resolves the OPERATION designator into an operation object.
   OPERATION is typically a symbol denoting an operation class, instantiated with MAKE-OPERATION.
2. Resolves the COMPONENT designator into a component object.
   COMPONENT is typically a string or symbol naming a system, loaded from disk using FIND-SYSTEM.
3. It then calls MAKE-PLAN with the operation and system as arguments.
4. Finally calls PERFORM-PLAN on the resulting plan to actually build the system.

The entire computation is wrapped in WITH-COMPILATION-UNIT and error handling code.
If a VERSION argument is supplied, then operate also ensures that the system found satisfies it
using the VERSION-SATISFIES method.
If a PLAN-CLASS argument is supplied, that class is used for the plan.

The :FORCE or :FORCE-NOT argument to OPERATE can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  (SYSTEM1 SYSTEM2 ... SYSTEMN) to force systems named in a given list
:FORCE-NOT has precedence over :FORCE; builtin systems cannot be forced.

For backward compatibility, all keyword arguments are passed to MAKE-OPERATION
when instantiating a new operation, that will in turn be inherited by new operations.
But do NOT depend on it, for this is deprecated behavior."))

  (define-convenience-action-methods operate (operation component &key)
    :if-no-component (error 'missing-component :requires component))

  (defvar *in-operate* nil
    "Are we in operate?")

  ;; This method ensures that an ASDF upgrade is attempted as the very first thing,
  ;; with suitable state preservation in case in case it actually happens,
  ;; and that a few suitable dynamic bindings are established.
  (defmethod operate :around (operation component &rest keys
                              &key verbose
                                (on-warnings *compile-file-warnings-behaviour*)
                                (on-failure *compile-file-failure-behaviour*) &allow-other-keys)
    (nest
     (with-asdf-cache ())
     (let ((in-operate *in-operate*)
           (*in-operate* t)
           (operation-remaker ;; how to remake the operation after ASDF was upgraded (if it was)
            (etypecase operation
              (operation (let ((name (type-of operation)))
                           #'(lambda () (make-operation name))))
              ((or symbol string) (constantly operation))))
           (component-path (typecase component ;; to remake the component after ASDF upgrade
                             (component (component-find-path component))
                             (t component)))))
     ;; Before we operate on any system, make sure ASDF is up-to-date,
     ;; for if an upgrade is ever attempted at any later time, there may be BIG trouble.
     (progn
       (unless in-operate
         (when (upgrade-asdf)
           ;; If we were upgraded, restart OPERATE the hardest of ways, for
           ;; its function may have been redefined.
           (return-from operate
             (apply 'operate (funcall operation-remaker) component-path keys)))))
      ;; Setup proper bindings around any operate call.
     (let* ((*verbose-out* (and verbose *standard-output*))
            (*compile-file-warnings-behaviour* on-warnings)
            (*compile-file-failure-behaviour* on-failure))
       (call-next-method))))

  (defmethod operate :before ((operation operation) (component component)
                              &key version &allow-other-keys)
    (unless (version-satisfies component version)
      (error 'missing-component-of-version :requires component :version version)))

  (defmethod operate ((operation operation) (component component)
                      &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-plan plan-class operation component keys)))
      (apply 'perform-plan plan keys)
      (values operation plan)))

  (defun oos (operation component &rest args &key &allow-other-keys)
    (apply 'operate operation component args))

  (setf (documentation 'oos 'function)
        (format nil "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
                (documentation 'operate 'function))))


;;;; Common operations
(when-upgrading ()
  (defmethod component-depends-on ((o prepare-op) (s system))
    (call-next-method)))
(with-upgradability ()
  (defclass build-op (non-propagating-operation) ()
    (:documentation "Since ASDF3, BUILD-OP is the recommended 'master' operation,
to operate by default on a system or component, via the function BUILD.
Its meaning is configurable via the :BUILD-OPERATION option of a component.
which typically specifies the name of a specific operation to which to delegate the build,
as a symbol or as a string later read as a symbol (after loading the defsystem-depends-on);
if NIL is specified (the default), BUILD-OP falls back to LOAD-OP,
that will load the system in the current image."))
  (defmethod component-depends-on ((o build-op) (c component))
    `((,(or (component-build-operation c) 'load-op) ,c)
      ,@(call-next-method)))

  (defun make (system &rest keys)
    "The recommended way to interact with ASDF3.1 is via (ASDF:MAKE :FOO).
It will build system FOO using the operation BUILD-OP,
the meaning of which is configurable by the system, and
defaults to LOAD-OP, to load it in current image."
    (apply 'operate 'build-op system keys)
    t)

  (defun load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate 'load-op system keys)
    t)

  (defun load-systems* (systems &rest keys)
    "Loading multiple systems at once."
    (dolist (s systems) (apply 'load-system s keys)))

  (defun load-systems (&rest systems)
    "Loading multiple systems at once."
    (load-systems* systems))

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

;;;;; Define the function REQUIRE-SYSTEM, that, similarly to REQUIRE,
;; only tries to load its specified target if it's not loaded yet.
(with-upgradability ()
  (defun component-loaded-p (component)
    "Has the given COMPONENT been successfully loaded in the current image (yet)?
Note that this returns true even if the component is not up to date."
    (if-let ((component (find-component component () :registered t)))
      (action-already-done-p nil (make-operation 'load-op) component)))

  (defun already-loaded-systems ()
    "return a list of the names of the systems that have been successfully loaded so far"
    (mapcar 'coerce-name (remove-if-not 'component-loaded-p (registered-systems*))))

  (defun require-system (system &rest keys &key &allow-other-keys)
    "Ensure the specified SYSTEM is loaded, passing the KEYS to OPERATE, but do not update the
system or its dependencies if they have already been loaded."
    (unless (component-loaded-p system)
      (apply 'load-system system :force-not (already-loaded-systems) keys))))


;;;; Define the class REQUIRE-SYSTEM, to be hooked into CL:REQUIRE when possible,
;; i.e. for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL
;; Note that despite the two being homonyms, the _function_ require-system
;; and the _class_ require-system are quite distinct entities, fulfilling independent purposes.
(with-upgradability ()
  (defvar *modules-being-required* nil)

  (defclass require-system (system)
    ((module :initarg :module :initform nil :accessor required-module))
    (:documentation "A SYSTEM subclass whose processing is handled by
the implementation's REQUIRE rather than by internal ASDF mechanisms."))

  (defmethod perform ((o compile-op) (c require-system))
    nil)

  (defmethod perform ((o load-op) (s require-system))
    (let* ((module (or (required-module s) (coerce-name s)))
           (*modules-being-required* (cons module *modules-being-required*)))
      (assert (null (component-children s)))
      (require module)))

  (defmethod resolve-dependency-combination (component (combinator (eql :require)) arguments)
    (unless (and (length=n-p arguments 1)
                 (typep (car arguments) '(or string (and symbol (not null)))))
      (parameter-error (compatfmt "~@<In ~S, bad dependency ~S for ~S. ~S takes one argument, a string or non-null symbol~@:>")
                       'resolve-dependency-combination
                       (cons combinator arguments) component combinator))
    ;; :require must be prepared for some implementations providing modules using ASDF,
    ;; as SBCL used to do, and others may might do. Thus, the system provided in the end
    ;; would be a downcased name as per module-provide-asdf above. For the same reason,
    ;; we cannot assume that the system in the end will be of type require-system,
    ;; but must check whether we can use find-system and short-circuit cl:require.
    ;; Otherwise, calling cl:require could result in nasty reentrant calls between
    ;; cl:require and asdf:operate that could potentially blow up the stack,
    ;; all the while defeating the consistency of the dependency graph.
    (let* ((module (car arguments)) ;; NB: we already checked that it was not null
           ;; CMUCL, MKCL, SBCL like their module names to be all upcase.
           (module-name (string module))
           (system-name (string-downcase module))
           (system (find-system system-name nil)))
      (or system (let ((system (make-instance 'require-system :name system-name :module module-name)))
                   (register-system system)
                   system))))

  (defun module-provide-asdf (name)
    ;; We must use string-downcase, because modules are traditionally specified as symbols,
    ;; that implementations traditionally normalize as uppercase, for which we seek a system
    ;; with a name that is traditionally in lowercase. Case is lost along the way. That's fine.
    ;; We could make complex, non-portable rules to try to preserve case, and just documenting
    ;; them would be a hell that it would be a disservice to inflict on users.
    (let ((module-name (string name))
          (system-name (string-downcase name)))
      (unless (member module-name *modules-being-required* :test 'equal)
        (let ((*modules-being-required* (cons module-name *modules-being-required*))
              #+sbcl (sb-impl::*requiring* (remove module-name sb-impl::*requiring* :test 'equal)))
          (handler-bind
              ((style-warning #'muffle-warning)
               (missing-component (constantly nil))
               (fatal-condition
                #'(lambda (e)
                    (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                            name e))))
            (let ((*verbose-out* (make-broadcast-stream)))
              (let ((system (find-system system-name nil)))
                (when system
                  (require-system system-name :verbose nil)
                  t)))))))))


;;;; Some upgrade magic
(with-upgradability ()
  (defun restart-upgraded-asdf ()
    ;; If we're in the middle of something, restart it.
    (let ((systems-being-defined
           (when *asdf-cache*
             (prog1
                 (loop :for k :being :the hash-keys :of *asdf-cache*
                   :when (eq (first k) 'find-system) :collect (second k))
               (clrhash *asdf-cache*)))))
      ;; Regardless, clear defined systems, since they might be invalid
      ;; after an incompatible ASDF upgrade.
      (clear-defined-systems)
      ;; The configuration also may have to be upgraded.
      (upgrade-configuration)
      ;; If we were in the middle of an operation, be sure to restore the system being defined.
      (dolist (s systems-being-defined) (find-system s nil))))
  (register-hook-function '*post-upgrade-cleanup-hook* 'restart-upgraded-asdf)

  ;; The following function's symbol is from asdf/find-system.
  ;; It is defined here to resolve what would otherwise be forward package references.
  (defun mark-component-preloaded (component)
    "Mark a component as preloaded."
    (let ((component (find-component component nil :registered t)))
      ;; Recurse to children, so asdf/plan will hopefully be happy.
      (map () 'mark-component-preloaded (component-children component))
      ;; Mark the timestamps of the common lisp-action operations as 0.
      (let ((times (component-operation-times component)))
        (dolist (o '(load-op compile-op prepare-op))
          (setf (gethash (make-operation o) times) 0))))))

