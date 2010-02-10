(asdf:defsystem :test-module-depend
  :components ((:file "file1")
               (:module "quux"
                        :pathname #p""
                        :depends-on ("file1")
                        :components ((:file "file2")
                                     (:module "file3mod"
                                              :pathname #p""
                                              :components
                                              ((:file "file3")))))))

