;; Example executable program

#-mkcl
(defsystem :hello-world-example
  :build-operation program-op
  :entry-point "hello:entry-point"
  :depends-on (:uiop)
  :components ((:file "hello")))

#+mkcl
(defsystem :hello-world-example
  :class asdf/bundle::program
  :build-operation program-op
  :entry-point "hello:entry-point"
  :depends-on (:uiop)
  :components ((:file "hello"))
  :prefix-lisp-object-files #.(list (namestring (truename (translate-logical-pathname #P"SYS:cmp.a"))))
  :extra-build-args #.(or #-windows '(:use-mkcl-shared-libraries nil))
  :epilogue-code (progn
                   (setq uiop/image:*image-dumped-p* :executable)
                   (uiop/image:restore-image :entry-point (read-from-string "hello:entry-point"))))

