(in-package :asdf-test/deferred-warnings)

(defun use-setf-foo (v x)
  (setf (foo x) v))
