(defsystem test-builtin-source-file-type-1
    :default-component-class cl-source-file.cl
    :serial t
    :components ((:cl-source-file "file1") ; for the package
                 (:file "test-tmp")))
