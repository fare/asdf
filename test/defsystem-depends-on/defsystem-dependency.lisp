(in-package :asdf-test)
(defvar *dd* 0)
(incf *dd*)
(format t "Loaded defsystem-dependency.lisp ~d time~(~:*~p~)~%" *dd*)
(setf (find-class 'asdf::my-cl-source-file) (find-class 'cl-source-file))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cdd* 0))

(eval-when (:compile-toplevel :execute)
  (incf *cdd*))
