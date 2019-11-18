(defclass never-done (cl-source-file)
  ())

(defmethod operation-done-p ((op load-op) (c never-done))
  NIL)

(defmethod operation-done-p ((op compile-op) (c never-done))
  NIL)


(defsystem "test-operation-done-p"
  :serial t
  :default-component-class never-done
  :components
  ((:file "file1")
   (:file "file2")))
