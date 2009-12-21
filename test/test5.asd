;;; -*- Lisp -*-
(asdf:defsystem test5
  :components
  ((:module "deps"
            :pathname "."
            :components
            ((:file "file1")
             (:file "file2" :load-only-p t)))))

