(defsystem test-builtin-source-file-type-4
  :default-component-class cl-source-file.lsp
  :serial t
  :components ((:file "file1" :type "lisp") ; for package
               (:file "test-tmp")))
