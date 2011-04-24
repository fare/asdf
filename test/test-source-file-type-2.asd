;; Works only in ASDF 2
(defclass cl-source-file-2 (cl-source-file)
   ((type :initform "cl")))

(defsystem test-source-file-type-2
    :depends-on (:test-source-file-type-1)
    :default-component-class cl-source-file-2
    :components ((:file "test-tmp")))
