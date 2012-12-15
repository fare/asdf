;;; -*- Lisp -*-
(in-package :asdf)

(defun call-in-base-2 (thunk)
  (let ((*read-base* 2))
    (funcall thunk)))

;;(trace call-in-base-2 compile-file load trace around-compile-hook)

(defsystem test-around-compile
  :around-compile call-in-base-2
  :depends-on ((:version :asdf "2.017.18")) ; no :around-compile before that.
  :components ((:file "test")))
