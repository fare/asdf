;;;; -------------------------------------------------------------------------
;;;; Operations

(uiop/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf/action :asdf) ;; asdf/action for FEATURE pre 2.31.5.
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:operation
   #:operation-original-initargs #:original-initargs ;; backward-compatibility only. DO NOT USE.
   #:*operations* #:make-operation #:find-operation #:feature))
(in-package :asdf/operation)

;;; Operation Classes

(when-upgrading (:when (find-class 'operation nil))
  ;; override any obsolete shared-initialize method when upgrading from ASDF2.
  (defmethod shared-initialize :after ((o operation) (slot-names t) &key)
    (values)))

(with-upgradability ()
  (defclass operation ()
    ((original-initargs ;; for backward-compat -- used by GBBopen and swank (via operation-forced)
      :initform nil :initarg :original-initargs :accessor operation-original-initargs)))

  ;; Cache a copy of the INITARGS in the ORIGINAL-INITARGS slot, if that slot is not
  ;; already bound.
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
  (defparameter* *operations* (make-hash-table :test 'equal))

  (defun make-operation (operation-class &rest initargs)
    (ensure-gethash (cons operation-class initargs) *operations*
                    (list* 'make-instance operation-class initargs)))

  (defgeneric find-operation (context spec)
    (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
  (defmethod find-operation ((context t) (spec operation))
    spec)
  (defmethod find-operation (context (spec symbol))
    (unless (member spec '(nil feature))
      ;; NIL designates itself, i.e. absence of operation
      ;; FEATURE is the ASDF1 misfeature that comes with IF-COMPONENT-DEP-FAILS
      (apply 'make-operation spec (operation-original-initargs context))))
  (defmethod operation-original-initargs ((context symbol))
    (declare (ignorable context))
    nil))

