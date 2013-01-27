;;; -*- mode: lisp -*-
(in-package :asdf)

(defun call-without-redefinition-warnings (thunk)
  (handler-bind (#+clozure (ccl:compiler-warning #'muffle-warning))
    (funcall thunk)))

(defsystem :asdf-driver
  :licence "MIT"
  :description "Runtime support for Common Lisp programs"
  :long-description "Basic general-purpose utilities that are in such a need
that you can't portably construct a complete program without using them."
  #+asdf3 :version #+asdf3 (:read-file-form "version.lisp-expr")
  :around-compile call-without-redefinition-warnings
  :components
  ((:static-file "version.lisp-expr")
   (:static-file "contrib/debug.lisp")
   (:file "package")
   (:file "common-lisp" :depends-on ("package"))
   (:file "utility" :depends-on ("common-lisp"))
   (:file "pathname" :depends-on ("utility"))
   (:file "stream" :depends-on ("pathname"))
   (:file "os" :depends-on ("stream"))
   (:file "image" :depends-on ("os"))
   (:file "run-program" :depends-on ("os"))
   (:file "lisp-build" :depends-on ("image"))
   (:file "configuration" :depends-on ("image"))
   (:file "backward-driver" :depends-on ("lisp-build" "run-program" "configuration"))
   (:file "driver" :depends-on ("backward-driver"))))
