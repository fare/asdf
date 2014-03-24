;;; -*- Lisp -*-
#+lispworks (lispworks:load-all-patches)
(load (make-pathname :name "script-support" :defaults *load-pathname*))
(load-asdf)
#+(or ecl mkcl) (require :cmp)

(asdf-test::register-directory asdf-test::*asdf-directory*) ;; we need UIOP, and ECL can dump.
(asdf-test::register-directory asdf-test::*uiop-directory*)
(asdf:upgrade-asdf) ;; may recompile and rename away package asdf?

(asdf-test::frob-packages)

(defun make-hello-image ()
  (operate 'load-fasl-op :hello-world-example)
  (operate 'image-op :hello-world-example))

(defun make-hello-program ()
  (operate 'load-fasl-op :hello-world-example)
  (operate 'program-op :hello-world-example)
  #+(and mkcl windows) ;; make sure mkcl-X.X.X.dll is the same directory as the executable
  (let* ((dll-orig (subpathname (si::self-truename)
                                (strcat "mkcl_" (lisp-implementation-version) ".dll")))
         (exe (asdf::output-file 'program-op :hello-world-example))
         (dll-dest (subpathname exe dll-orig)))
    (copy-file dll-orig dll-dest)))
