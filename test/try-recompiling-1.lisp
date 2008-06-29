(in-package #:common-lisp-user)

(defun try-recompiling-1 ()
  (assert *caught-error*))

(try-recompiling-1)

