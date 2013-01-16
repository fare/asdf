;;; -*- mode: lisp -*-

(defsystem :generate-asdf
  :licence "MIT"
  :description "All the components needed to build asdf.lisp"
  :description "Generate asdf.lisp based on this and monolithic-concatenate-source-op"
  ;; :defsystem-depends-on (:asdf/bundle)
  :class :concatenated-source-system
  ;; :include-dependencies t
  :translate-output-p nil
  :concatenated-source-file "build/asdf"
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on (:asdf-driver)
  :components
  ((:file "upgrade")
   (:file "component")
   (:file "system" :depends-on ("component"))
   (:file "find-system" :depends-on ("system"))
   (:file "find-component" :depends-on ("find-system"))
   (:file "operation")
   (:file "action" :depends-on ("find-component" "operation"))
   (:file "lisp-action" :depends-on ("action" "lisp-build"))
   (:file "plan" :depends-on ("action"))
   (:file "operate" :depends-on ("plan"))
   (:file "output-translations" :depends-on ("operate"))
   (:file "source-registry" :depends-on ("find-system"))
   (:file "backward-internals" :depends-on ("action" "operate"))
   (:file "defsystem" :depends-on ("backward-internals"))
   (:file "bundle" :depends-on ("lisp-action"))
   (:file "concatenate-source" :depends-on ("lisp-action"))
   (:file "backward-interface" :depends-on ("lisp-action"))
   (:file "interface")
   (:file "footer" :depends-on ("interface"))))
