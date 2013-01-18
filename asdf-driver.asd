;;; -*- mode: lisp -*-
(in-package :asdf)

(defun call-without-redefinition-warnings (thunk)
  (handler-bind (#+clozure (ccl:compiler-warning #'muffle-warning))
    (funcall thunk)))

(defsystem :asdf-driver
  :licence "MIT"
  :description "Basic general-purpose utilities used by ASDF"
  :long-description "Basic general-purpose utilities that is in such a need
that you can't portably construct a complete program without using them."
  #+asdf2.27 :version #+asdf2.27 (:read-file-form "version.lisp-expr")
  :defsystem-depends-on (#+(and (not asdf2.27) (or clisp xcl)) :asdf)
  :around-compile call-without-redefinition-warnings
  :components
  ((:file "header")
   (:file "package")
   (:file "compatibility" :depends-on ("package"))
   (:file "utility" :depends-on ("compatibility"))
   (:file "pathname" :depends-on ("utility"))
   (:file "stream" :depends-on ("pathname"))
   (:file "os" :depends-on ("stream"))
   (:file "image" :depends-on ("os"))
   (:file "run-program" :depends-on ("os"))
   (:file "lisp-build" :depends-on ("image"))
   (:file "configuration" :depends-on ("image"))
   (:file "backward-driver" :depends-on ("lisp-build" "run-program" "configuration"))
   (:file "driver" :depends-on ("backward-driver"))))
