;; Example executable program

(defsystem :hello-world-example
  :class :program-system
  :entry-point "hello:main"
  :depends-on (:asdf-driver)
  :translate-output-p nil
  :components ((:file "hello")))
