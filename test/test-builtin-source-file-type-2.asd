(defsystem test-builtin-source-file-type-2
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "file1" :type "lisp") ; for package
               (:file "test-tmp")))
