;;;---------------------------------------------------------------------------
;;; This system is to be used to test the generation of ASD files by
;;; DELIVER-ASD-OP.  See test-bundle-asd.script.
;;;---------------------------------------------------------------------------


(defsystem :deliver-bundle
    :defsystem-depends-on ("file3-only")
  :depends-on ("test1")          ; brings in "file1" and "file2"
  :components ((:file "file4")))
