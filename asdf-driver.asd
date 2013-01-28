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
  #+asdf3 :encoding #+asdf3 :utf-8
  :around-compile call-without-redefinition-warnings
  :components
  ((:static-file "version.lisp-expr")
   (:static-file "contrib/debug.lisp")
   (:file "package")
   (:file "common-lisp" :depends-on ("package"))
   (:file "utility" :depends-on ("common-lisp"))
   (:file "os" :depends-on ("utility"))
   (:file "pathname" :depends-on ("os"))
   (:file "stream" :depends-on ("pathname"))
   (:file "image" :depends-on ("stream"))
   (:file "run-program" :depends-on ("stream"))
   (:file "lisp-build" :depends-on ("image"))
   (:file "configuration" :depends-on ("image"))
   (:file "backward-driver" :depends-on ("lisp-build" "run-program" "configuration"))
   (:file "driver" :depends-on ("backward-driver"))))
