(in-package :asdf-test/deferred-warnings)

(defun use-setf-foo (v x)
  (setf (foo x) v))

#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
          (format t "~&~S~%" `(sys::*unknown-functions* ,sys::*unknown-functions*)))
