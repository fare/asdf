;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(asdf/package:define-package :asdf/operate
  (:recycle :asdf/operate :asdf)
  (:use :common-lisp :asdf/implementation :asdf/utility :asdf/upgrade
        :asdf/component :asdf/system :asdf/operation :asdf/action
        :asdf/lisp-build :asdf/lisp-action #:asdf/plan
        :asdf/find-system :asdf/find-component)
  (:export
   #:operate #:oos #:*systems-being-operated*
   #:load-system #:load-systems #:compile-system #:test-system #:require-system
   #:*load-system-operation* #:module-provide-asdf
   #:component-loaded-p #:already-loaded-systems
   #:upgrade-asdf #:cleanup-upgraded-asdf #:*post-upgrade-hook*))
(in-package :asdf/operate)

(defgeneric* operate (operation-class system &key &allow-other-keys))

(defun* cleanup-upgraded-asdf ()
  (let ((asdf (funcall (find-symbol* 'find-system :asdf) :asdf)))
    ;; Invalidate all systems but ASDF itself.
    (setf *defined-systems* (make-defined-systems-table))
    (register-system asdf)
    (funcall (find-symbol* 'load-system :asdf) :asdf))) ;; load ASDF a second time, the right way.

(defun* restart-upgraded-asdf ()
  ;; If we're in the middle of something, restart it.
  (when *systems-being-defined*
    (let ((l (loop :for name :being :the :hash-keys :of *systems-being-defined* :collect name)))
      (clrhash *systems-being-defined*)
      (dolist (s l) (find-system s nil)))))

(pushnew 'cleanup-upgraded-asdf *post-upgrade-cleanup-hook*)
(pushnew 'restart-upgraded-asdf *post-upgrade-restart-hook*)


;;;; Operate itself

(defvar *systems-being-operated* nil
  "A boolean indicating that some systems are being operated on")

(defmethod operate :around (operation-class system
                            &key verbose
                              (on-warnings *compile-file-warnings-behaviour*)
                              (on-failure *compile-file-failure-behaviour*) &allow-other-keys)
  (declare (ignorable operation-class system))
  (with-system-definitions ()
    (let* ((*asdf-verbose* verbose)
           (*verbose-out* (if verbose *standard-output* (make-broadcast-stream)))
           (*compile-file-warnings-behaviour* on-warnings)
           (*compile-file-failure-behaviour* on-failure))
      (call-next-method))))

(defmethod operate (operation-class system &rest args &key version &allow-other-keys)
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
:FORCE has precedence over :FORCE-NOT; builtin systems cannot be forced."
  (let* ((system (etypecase system
                   (system system)
                   ((or string symbol) (find-system system))))
         ;; I'd like to remove-keys :force :force-not :verbose, but swank.asd relies on :force (!).
         (op (apply 'make-operation operation-class args))
         (systems-being-operated *systems-being-operated*)
         (*systems-being-operated* (or systems-being-operated (make-hash-table :test 'equal))))
    (check-type system system)
    (setf (gethash (coerce-name system) *systems-being-operated*) system)
    (flet ((upgrade ()
             ;; If we needed to upgrade ASDF to achieve our goal,
             ;; then do it specially as the first thing,
             ;; which will invalidate all existing systems;
             ;; afterwards, try again with the new OPERATE function,
             ;; which on some implementations may be a new symbol.
             (unless (gethash "asdf" *systems-being-operated*)
               (upgrade-asdf)
               (return-from operate
                 (apply (find-symbol* 'operate :asdf) operation-class system args)))))
      (when systems-being-operated ;; Upgrade if loading a system from another one.
        (upgrade))
      (unless (version-satisfies system version)
        (error 'missing-component-of-version :requires system :version version))
      (let ((plan (apply 'traverse op system args)))
        (when (plan-operates-on-p plan '("asdf"))
          (upgrade)) ;; Upgrade early if the plan involves upgrading asdf at any time.
        (perform-plan plan)
        (values op plan)))))

(defun* oos (operation-class system &rest args
             &key force force-not verbose version &allow-other-keys)
  (declare (ignore force force-not verbose version))
  (apply 'operate operation-class system args))

(setf (documentation 'oos 'function)
      (format nil "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
              (documentation 'operate 'function)))


;;;; Common operations

(defvar *load-system-operation* 'load-op
  "Operation used by ASDF:LOAD-SYSTEM. By default, ASDF:LOAD-OP.
You may override it with e.g. ASDF:LOAD-FASL-OP from asdf-bundle,
or ASDF:LOAD-SOURCE-OP if your fasl loading is somehow broken.")

(defun* load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'operate *load-system-operation* system keys)
  t)

(defun* load-systems (&rest systems)
  (map () 'load-system systems))

(defun* compile-system (system &rest args &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(asdf:operate 'asdf:compile-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'operate 'compile-op system args)
  t)

(defun* test-system (system &rest args &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(asdf:operate 'asdf:test-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'operate 'test-op system args)
  t)


;;;; require-system, and hooking it into CL:REQUIRE when possible,
;; i.e. for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL

(defun* component-loaded-p (c)
  (action-already-done-p nil (make-instance 'load-op) (find-component c ())))

(defun* already-loaded-systems ()
  (remove-if-not 'component-loaded-p (registered-systems)))

(defun* require-system (s &rest keys &key &allow-other-keys)
  (apply 'load-system s :force-not (already-loaded-systems) keys))
  
(defun* module-provide-asdf (name)
  (handler-bind
      ((style-warning #'muffle-warning)
       #-genera
       (missing-component (constantly nil))
       (error #'(lambda (e)
                  (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                          name e))))
    (let ((*verbose-out* (make-broadcast-stream))
          (system (find-system (string-downcase name) nil)))
      (when system
        (require-system system :verbose nil)
        t))))

