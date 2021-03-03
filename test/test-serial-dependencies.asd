;;; This test example shows a failure in propagating serial dependencies across :if-feature
;;; conditionalization.

(asdf:defsystem test-serial-dependencies
    :serial t
  :components ((:file "file1")
               (:file "file4" :if-feature :undef)
               (:file "file3" :if-feature :undef)
               ;; we expect an error loading here, because file1 won't be loaded
               (:file "file2")))
