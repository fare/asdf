(defsystem test-builtin-source-file-type-3
    :default-component-class cl-source-file.lsp
    :serial t
    :components ((:cl-source-file "file1") ; for the package
                 (:file "test-tmp")))
