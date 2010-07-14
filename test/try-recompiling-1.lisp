(in-package #:common-lisp-user)

#+clisp (eval-when (:compile-toplevel) (fmakunbound 'try-recompiling-1))

(defun try-recompiling-1 ()
  (assert *caught-error*))

(try-recompiling-1)

