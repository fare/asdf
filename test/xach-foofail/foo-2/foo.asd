;;;; foo.asd

(asdf:defsystem #:foo
  :serial t
  :components ((:file "a")
               (:file "b")))
