;;; -*- Lisp -*-
#+lispworks (lispworks:load-all-patches)
(load (make-pathname :name "script-support" :defaults *load-pathname*))
(load-asdf)
#+ecl (require :cmp)

(with-test ()
  (register-directory *asdf-directory*) ;; we need asdf-driver, and ECL can dump.
  (register-directory (subpathname *asdf-directory* "uiop/"))
  (operate 'load-fasl-op :hello-world-example)
  (operate 'program-op :hello-world-example))
