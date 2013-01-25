;;;; -------------------------------------------------------------------------
;;;; Stamp cache

(asdf/package:define-package :asdf/stamp-cache
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade)
  (:export #:get-file-stamp #:compute-file-stamp #:register-file-stamp
           #:call-with-stamp-cache #:with-stamp-cache #:*stamp-cache*))
(in-package :asdf/stamp-cache)

;;; This stamp cache is useful for:
;; * consistency of stamps used within a single run
;; * fewer accesses to the filesystem
;; * the ability to test with fake timestamps, without touching files

(defvar *stamp-cache* nil)

(defun compute-file-stamp (file)
  (safe-file-write-date file))

(defun register-file-stamp (file &optional (stamp (compute-file-stamp file)))
  (if *stamp-cache*
      (setf (gethash file *stamp-cache*) stamp)
      stamp))

(defun get-file-stamp (file)
  (if *stamp-cache*
      (multiple-value-bind (stamp foundp) (gethash file *stamp-cache*)
        (if foundp
            stamp
            (register-file-stamp file)))
      (compute-file-stamp file)))

(defun call-with-stamp-cache (thunk &key override)
  (if (and *stamp-cache* (not override))
      (funcall thunk)
      (let ((*stamp-cache* (make-hash-table :test 'equal)))
        (funcall thunk))))

(defmacro with-stamp-cache ((&key override) &body body)
  `(call-with-stamp-cache #'(lambda () ,@body) :override ,override))
