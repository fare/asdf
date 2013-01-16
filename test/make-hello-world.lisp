;;; -*- Lisp -*-
(load "script-support.lisp")
(load-asdf)

(with-test ()
  (operate 'program-op :hello-world-example))
