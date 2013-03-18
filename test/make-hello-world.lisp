;;; -*- Lisp -*-
#+lispworks (lispworks:load-all-patches)
(load (make-pathname :name "script-support" :defaults *load-pathname*))
(load-asdf)
#+ecl (require :cmp)
#+ecl (trace uiop:compile-file* uiop:load*)

(asdf-test::register-directory asdf-test::*asdf-directory*) ;; we need asdf-driver, and ECL can dump.
(asdf-test::register-directory asdf-test::*uiop-directory*)
(asdf:upgrade-asdf) ;; may recompile and rename away package asdf?

(asdf-test::frob-packages)

(with-test ()
  ;;(dolist (s '(:asdf :asdf/driver :asdf/defsystem :uiop)) (DBG :foo s (asdf::builtin-system-p (find-system s))))
  (trace perform-plan perform)
  (operate 'load-fasl-op :hello-world-example)
  (operate 'program-op :hello-world-example))
