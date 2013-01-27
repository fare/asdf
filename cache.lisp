;;;; -------------------------------------------------------------------------
;;;; Stamp cache

(asdf/package:define-package :asdf/cache
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade)
  (:export #:get-file-stamp #:compute-file-stamp #:register-file-stamp
           #:consult-asdf-cache #:do-asdf-cache
           #:call-with-asdf-cache #:with-asdf-cache #:*asdf-cache*))
(in-package :asdf/cache)

;;; This stamp cache is useful for:
;; * consistency of stamps used within a single run
;; * fewer accesses to the filesystem
;; * the ability to test with fake timestamps, without touching files

(defvar *asdf-cache* nil)

(defun set-asdf-cache-entry (key value-list)
  (apply 'values
         (if *asdf-cache*
             (setf (gethash key *asdf-cache*) value-list)
             value-list)))

(defun consult-asdf-cache (key thunk)
  (if *asdf-cache*
      (multiple-value-bind (results foundp) (gethash key *asdf-cache*)
        (if foundp
            (apply 'values results)
            (set-asdf-cache-entry key (multiple-value-list (funcall thunk)))))
      (funcall thunk)))

(defmacro do-asdf-cache (key &body body)
  `(consult-asdf-cache ,key #'(lambda () ,@body)))

(defun call-with-asdf-cache (thunk &key override)
  (if (and *asdf-cache* (not override))
      (funcall thunk)
      (let ((*asdf-cache* (make-hash-table :test 'equal)))
        (funcall thunk))))

(defmacro with-asdf-cache ((&key override) &body body)
  `(call-with-asdf-cache #'(lambda () ,@body) :override ,override))

(defun compute-file-stamp (file)
  (safe-file-write-date file))

(defun register-file-stamp (file &optional (stamp (compute-file-stamp file)))
  (set-asdf-cache-entry `(get-file-stamp ,file) (list stamp)))

(defun get-file-stamp (file)
  (do-asdf-cache `(get-file-stamp ,file) (compute-file-stamp file)))
