#|
make sure that serial t and static-files don't cause full rebuilds all
the time...
|#

(defsystem static-and-serial
  :version "0.1"
  :serial t
  :components
  ((:static-file "file2.lisp")
   (:static-file "run-tests.sh")
   (:file "file1")))
