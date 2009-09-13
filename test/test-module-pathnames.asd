;;; -*- Lisp -*-

(asdf:defsystem test-module-pathnames
  :components
  ((:module "sources/level1"
    :serial t
	    :components
	    ((:file "file1")
	     (:file "level2/file2")
	     (:static-file "test-tmp.cl")))))

