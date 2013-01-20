;;;; -------------------------------------------------------------------------
;;;; Operations

(asdf/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade)
  (:export
   #:operation
   #:operation-original-initargs ;; backward-compatibility only. DO NOT USE.
   #:build-op ;; THE generic operation
   #:make-operation
   #:find-operation))
(in-package :asdf/operation)

;;; Operation Classes

(when-upgrade (:when (find-class 'operation nil))
  (defmethod shared-initialize :after ((o operation) slot-names &rest initargs &key)
    (declare (ignorable o slot-names initargs)) (values)))

(defclass operation ()
  ((original-initargs ;; for backward-compat -- used by GBBopen and swank (via operation-forced)
    :initform nil :initarg :original-initargs :accessor operation-original-initargs)))

(defmethod initialize-instance :after ((o operation) &rest initargs
                                       &key force force-not system verbose &allow-other-keys)
  (declare (ignorable force force-not system verbose))
  (unless (slot-boundp o 'original-initargs)
    (setf (operation-original-initargs o) initargs)))

(defmethod print-object ((o operation) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (ignore-errors
      (format stream "~{~S~^ ~}" (operation-original-initargs o)))))

;;; make-operation, find-operation

(defparameter *operations* (make-hash-table :test 'equal))
(defun* make-operation (operation-class &rest initargs)
  (let ((key (cons operation-class initargs)))
    (multiple-value-bind (operation foundp) (gethash key *operations*)
    (if foundp operation
        (setf (gethash key *operations*)
              (apply 'make-instance operation-class initargs))))))

(defgeneric* find-operation (context spec)
  (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
(defmethod find-operation (context (spec operation))
  (declare (ignorable context))
  spec)
(defmethod find-operation (context (spec symbol))
  (apply 'make-operation spec (operation-original-initargs context)))
(defmethod operation-original-initargs ((context symbol))
  (declare (ignorable context))
  nil)

(defclass build-op (operation) ())

