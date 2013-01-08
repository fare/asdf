;;; -*- mode: lisp -*-

(defsystem :generate-asdf
  :licence "MIT"
  :description "Generate a monolithic asdf.lisp from all its components"
  ;; :defsystem-depends-on (:asdf/bundle)
  :class :concatenated-source-system
  ;; :include-dependencies t
  :translate-output-p nil
  :output-file "tmp/asdf.lisp"
#|
  :depends-on (:asdf-header
               :asdf-package
               :asdf-portability
               :asdf-utility
               :asdf-pathname
               :asdf-upgrade
               :asdf-interface
               :asdf-lisp-build
               :asdf-action
               :asdf-bundle
               :asdf-backward-compat
               :asdf-footer))
|#
  :serial t
  :components
  ((:file "header")
   (:module
    "code" :pathname "" :components
    ((:file "package")
     (:file "implementation" :depends-on ("package"))
     (:file "utility" :depends-on ("implementation"))
     (:file "pathname" :depends-on ("utility"))
     (:file "os" :depends-on ("pathname"))
     (:file "upgrade" :depends-on ("utility"))
     (:file "component" :depends-on ("pathname"))
     (:file "system" :depends-on ("component"))
     (:file "find-system" :depends-on ("system"))
     (:file "find-component" :depends-on ("find-system"))
     (:file "lisp-build" :depends-on ("pathname"))
     (:file "operation" :depends-on ("utility"))
     (:file "action" :depends-on ("find-component" "operation"))
     (:file "lisp-action" :depends-on ("action" "lisp-build"))
     (:file "plan" :depends-on ("action"))
     (:file "operate" :depends-on ("plan"))
     (:file "configuration" :depends-on ("pathname"))
     (:file "output-translations" :depends-on ("configuration" "operate"))
     (:file "source-registry" :depends-on ("configuration" "find-system"))
     (:file "backward-internals" :depends-on ("action" "operate"))
     (:file "defsystem" :depends-on ("backward-internals"))
     (:file "bundle" :depends-on ("lisp-action"))
     (:file "concatenate-source" :depends-on ("lisp-action"))
     (:file "backward-interface" :depends-on ("lisp-action"))))
   (:file "user")
   (:file "footer" :depends-on ("interface"))))
