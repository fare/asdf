;; Example executable program

(defsystem :hello-world-example
  :class :bundle-system
  :build-operation program-op
  :entry-point "hello:main"
  :depends-on (:asdf-driver)
  :translate-output-p nil
  :components ((:file "hello")))
