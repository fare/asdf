;;; -*- Lisp -*-
(asdf:defsystem test-multiple
  :components
  ((:file "file1")))

(asdf:defsystem test-multiple-too
  :components
  ((:file "file1")))

(asdf:defsystem test-multiple-free
  :components
  ((:file "file1")))
