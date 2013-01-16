;;; -*- Lisp -*-
(load "script-support.lisp")
(load-asdf)
#+ecl (require :cmp)

(with-test ()
  (register-directory *asdf-directory*) ;; we need asdf-driver, and ECL can dump.
  (operate 'program-op :hello-world-example))
