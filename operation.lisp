;;;; -------------------------------------------------------------------------
;;;; Operations

(uiop/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf/action :asdf) ;; asdf/action for FEATURE pre 2.31.5.
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export
   #:operation
   #:operation-original-initargs #:original-initargs ;; backward-compatibility only. DO NOT USE.
   #:*operations* #:make-operation #:find-operation
   #:feature)) ;; TODO: stop exporting the deprecated FEATURE feature.
(in-package :asdf/operation)

;;; Operation Classes

(when-upgrading (:when (find-class 'operation nil))
  ;; override any obsolete shared-initialize method when upgrading from ASDF2.
  (defmethod shared-initialize :after ((o operation) (slot-names t) &key)
    (values)))

(with-upgradability ()
  (defclass operation ()
    ((original-initargs ;; for backward-compat -- used by GBBopen, and swank (via operation-forced)
      :initform nil :initarg :original-initargs :accessor operation-original-initargs))
    (:documentation "The base class for all ASDF operations.

ASDF does NOT, never did and never will distinguish between multiple operations of the same class.
Therefore, all slots of all operations must have (:allocation class) and no initargs.

Any exceptions currently maintained for backward-compatibility are deprecated,
and support for them may be discontinued at any moment.
"))

  ;; Cache a copy of the INITARGS in the ORIGINAL-INITARGS slot, if that slot is not already bound.
  ;; This is a deprecated feature temporarily maintained for backward compatibility.
  ;; It will be removed at some point in the future.
  (defmethod initialize-instance :after ((o operation) &rest initargs
                                         &key force force-not system verbose &allow-other-keys)
    (declare (ignore force force-not system verbose))
    (unless (slot-boundp o 'original-initargs)
      (setf (operation-original-initargs o) initargs)))

  (defmethod print-object ((o operation) stream)
    (print-unreadable-object (o stream :type t :identity nil)
      (ignore-errors
       (format stream "~{~S~^ ~}" (operation-original-initargs o))))))

;;; make-operation, find-operation

(with-upgradability ()
  ;; A table to memoize instances of a given operation. There shall be only one.
  (defparameter* *operations* (make-hash-table :test 'equal))

  ;; A memoizing way of creating instances of operation.
  ;; All operations MUST created through this function.
  (defun make-operation (operation-class &rest initargs)
    "This function creates and memoizes an instance of OPERATION-CLASS.

Use of INITARGS is for backward compatibility and may be discontinued at any time."
    (let ((class (coerce-class operation-class
                               :package :asdf/interface :super 'operation :error 'sysdef-error)))
      (ensure-gethash (cons class initargs) *operations*
                      (list* 'make-instance class initargs))))

  ;; We preserve the operation-original-initargs of the context,
  ;; but only as an unsupported feature.
  ;; This is all done purely for the temporary sake of backwards compatibility.
  (defgeneric find-operation (context spec)
    (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
  (defmethod find-operation ((context t) (spec operation))
    spec)
  (defmethod find-operation (context (spec symbol))
    (when spec ;; NIL designates itself, i.e. absence of operation
      (apply 'make-operation spec (operation-original-initargs context))))
  (defmethod find-operation (context (spec string))
    (apply 'make-operation spec (operation-original-initargs context)))
  (defmethod operation-original-initargs ((context symbol))
    (declare (ignorable context))
    nil))

