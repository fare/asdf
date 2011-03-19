;;; -*- Lisp -*-

(defsystem :test-system-pathnames
  :pathname "sources/level1"
  :components
  ((:file "file1")
   (:file "file2" :pathname "level2/file2")
   (:static-file "level2/static.file")
   (:static-file "test-tmp.cl")))
