;;; -*- mode: lisp -*-

(defsystem :generate-asdf
  :licence "MIT"
  :description "Generate a monolithic asdf.lisp from all its components"
  ;; :defsystem-depends-on (:asdf/bundle)
  :class :concatenated-source-system
  ;; :include-dependencies t
  :translate-output-p nil
  :concatenated-source-file "tmp/generated-asdf.lisp"
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
    "utils" :pathname "" :components
    ((:file "package")
     (:file "compatibility" :depends-on ("package"))
     (:file "utility" :depends-on ("compatibility"))
     (:file "pathname" :depends-on ("utility"))
     (:file "os" :depends-on ("pathname"))))
   (:module
    "code" :pathname "" :components
     (:file "upgrade")
     (:file "component")
     (:file "system" :depends-on ("component"))
     (:file "find-system" :depends-on ("system"))
     (:file "find-component" :depends-on ("find-system"))
     (:file "lisp-build")
     (:file "operation")
     (:file "action" :depends-on ("find-component" "operation"))
     (:file "lisp-action" :depends-on ("action" "lisp-build"))
     (:file "plan" :depends-on ("action"))
     (:file "operate" :depends-on ("plan"))
     (:file "configuration")
     (:file "output-translations" :depends-on ("configuration" "operate"))
     (:file "source-registry" :depends-on ("configuration" "find-system"))
     (:file "backward-internals" :depends-on ("action" "operate"))
     (:file "defsystem" :depends-on ("backward-internals"))
     (:file "bundle" :depends-on ("lisp-action"))
     (:file "concatenate-source" :depends-on ("lisp-action"))
     (:file "backward-interface" :depends-on ("lisp-action"))))
   (:file "interface")
   (:file "footer" :depends-on ("interface"))))
