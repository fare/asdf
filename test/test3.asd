;;; -*- Lisp -*-
(asdf:defsystem test3
  :properties ((:prop1 . "value"))
  :components
  ((:file "file1" :if-feature :asdf)
   (:file "file2" :if-feature (:not :asdf))))
