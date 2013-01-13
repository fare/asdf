;;; -*- mode: lisp -*-

(defsystem :asdf-driver
  :licence "MIT"
  :description "Basic general-purpose utilities used by ASDF"
  :long-description "Basic general-purpose utilities that is in such a need
that you can't portably construct a complete program without using them."
  :components
  ((:file "header")
   (:file "package")
   (:file "compatibility" :depends-on ("package"))
   (:file "utility" :depends-on ("compatibility"))
   (:file "pathname" :depends-on ("utility"))
   (:file "stream" :depends-on ("pathname"))
   (:file "os" :depends-on ("pathname" "stream"))
   (:file "image" :depends-on ("os"))
   (:file "run-program" :depends-on ("os"))
   (:file "lisp-build" :depends-on ("image"))
   (:file "configuration" :depends-on ("os"))
   (:file "driver" :depends-on ("lisp-build" "run-program" "configuration"))))
