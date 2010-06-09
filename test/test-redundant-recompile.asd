;;; -*- Lisp -*-
(asdf:defsystem test-redundant-recompile
  :components ((:file "file2" :in-order-to ((compile-op (load-op "file1"))
                                            (load-op (load-op "file1"))))
               (:file "file1")))

#|
;;;This test system definition attempts to replicate the excess dependencies
;;;that seem to give rise to launchpad bug 590517
;;;(https://bugs.launchpad.net/asdf/+bug/590517)

1) from clean, check that all fasl files build and that some function
   defined in the second file is present

2) load again.  Check to make sure that nothing is recompiled.
|#
