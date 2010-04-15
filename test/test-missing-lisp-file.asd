;;; -*- Lisp -*-
(asdf:defsystem test-missing-lisp-file
  :components ((:file "file2" :in-order-to ((compile-op (load-op "fileMissing"))))
               (:file "fileMissing")))

