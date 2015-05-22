;;;; Useful information when failing.
(in-package :asdf-tools)

(defvar *fail-fast* nil)
(defvar *fail-verbose* nil)
(defvar *execution-context* nil) ;; context for debugging information
(defvar *failures* nil)
(defvar *failurep* nil)

;;;(defvar *failure-handler* nil)

(define-condition execution-error (error)
  ())

(defmethod print-object ((failure execution-error) stream)
  (if *print-escape*
      (print-unreadable-object (failure stream :type t :identity nil)
        (let ((*print-escape* nil))
          (print-object failure stream)))
      (princ "FAILED" stream)))

(define-condition execution-failure (execution-error)
  ((context :initform nil :initarg :context :reader failure-context)
   (reason :initarg :reason :reader failure-reason)))

(defmethod print-object ((failure execution-failure) stream)
  (if *print-escape* (call-next-method)
      (format stream "~@[In ~{~A~^, ~}: ~]~:[FAILED~;~:*~A~]"
              (reverse (remove nil (failure-context failure)))
              (failure-reason failure))))

(define-condition execution-failures (execution-error)
  ((failures :initform nil :initarg :failures :reader failure-list)))

(defmethod print-object ((failures execution-failures) stream)
  (if *print-escape* (call-next-method)
      (format stream "Failure~@[~P~:*~{~&~A~&~}~]"
              (reverse (failure-list failures)))))

(defun make-error (&optional reason &rest arguments)
  (etypecase reason
    (error reason)
    (string (make-condition 'simple-error
                            :format-control reason :format-arguments arguments))
    (null (make-failures))
    (symbol (apply 'make-condition reason arguments))))

(defun fail! (&rest reason-arguments)
  (error (apply 'make-error reason-arguments)))

(defun success (&rest values)
  "Return magic values that signal success"
  (cond
    ((successp values) (apply 'values values))
    ((failurep values) (apply 'values values))
    (t (apply 'values t '#:success values))))

(defun failure (&optional failures)
  "Return magic values that signal failure"
  (values nil '#:failure failures))

(defun successp (value-list)
  (and (length>=n-p value-list 2)
       (equal (subseq value-list 0 2)
              (multiple-value-list (success)))))

(defun success-values (value-list)
  (subseq value-list 2))

(defun failurep (value-list)
  (and (length>=n-p value-list 2)
       (equal (subseq value-list 0 2)
              (subseq (multiple-value-list (failure)) 0 2))))

(defun failure-failures (value-list)
  (third value-list))

(defun success-if (test &rest failure-args)
  "If TEST, return success, otherwise, return failure with FAILURE-ARGS"
  (if test
      (success)
      (apply 'fail! (or failure-args '("failed")))))

(defun failure-if (test &rest failure-args)
  "If not TEST, return success, otherwise, return failure with FAILURE-ARGS"
  (apply 'success-if (not test) failure-args))

(defun make-failure (&optional reason &rest arguments)
  (if (typep reason 'execution-error)
      reason
      (make-condition 'execution-failure
                      :context *execution-context*
                      :reason (apply 'make-error reason arguments))))

(defun make-failures (&optional list)
  (make-condition 'execution-failures :failures list))

(defmacro with-failure-context ((&key name (fail-fast t)) &body body)
  `(call-with-failure-context (lambda () ,@body) :name ,name :fail-fast ,fail-fast))

(defun register-failures (failure)
  (let ((failure (make-failure failure)))
    (etypecase failure
      (execution-failures
       (setf *failures* (append (failure-list failure) *failures*)))
      (execution-failure
       (push failure *failures*)))
    failure))

(defun call-with-failure-context (thunk &key name (fail-fast t))
  (let ((toplevel (null *execution-context*))
        (*execution-context* (cons name *execution-context*)))
    (labels ((compute ()
               (block nil
                 (handler-bind ((error (lambda (c)
                                         (register-failures c)
                                         (cond
                                           (toplevel (return (failure *failures*)))
                                           (fail-fast (error (make-failures)))
                                           (t (return (failure)))))))
                   (funcall thunk))))
             (doit ()
               (let ((results (multiple-value-list (compute))))
                 (if (and toplevel *failures*) (failure (reverse *failures*)) (apply 'success results)))))
      (if toplevel
          (let ((*failures* nil)) (doit))
          (doit)))))

(defmacro without-stopping (() &body body)
  `(call-without-stopping (list ,@(mapcar (lambda (form) `(lambda () ,form)) body))))

(defun call-without-stopping (thunks)
  (with-failure-context (:fail-fast nil)
    (let ((failurep nil))
      (dolist (thunk thunks)
        (when (failurep (multiple-value-list (with-failure-context () (funcall thunk))))
          (setf failurep t)))
      (failure-if failurep (make-failures)))))

(defun run-command (fun &rest args)
  (let ((results (multiple-value-list (with-failure-context () (apply fun (rest args))))))
    ;; Don't print anything on success for regular commands, otherwise print all values returned.
    (if (failurep results)
        (let ((failures (failure-failures results)))
          (format t "~&Failure~P:~{~& ~A~}~&" (length failures) failures))
        (format t "~{~&~S~&~}" (if (successp results) (success-values results) results)))
    (apply 'values results)))
