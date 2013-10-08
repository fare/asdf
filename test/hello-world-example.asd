;; Example executable program

(defsystem :hello-world-example
  :build-operation program-op
  :entry-point "hello:entry-point"
  :depends-on (:uiop)
  :components ((:file "hello")))
