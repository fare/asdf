;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2012 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

(defsystem :asdf
  :author ("Daniel Barlow")
  :maintainer ("Francois-Rene Rideau")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "2.26.141" ;; to be automatically updated by make bump-version
  :depends-on ()
  :components ((:module "build" :components ((:file "asdf"))))
  :in-order-to (#+asdf2.27 (compile-op (monolithic-load-concatenated-source-op asdf/defsystem))))

#-asdf2.27
(defmethod perform :before
    ((o compile-op)
     (c (eql (first (module-components
                     (first (module-components (find-system :asdf))))))))
  (declare (ignorable o))
  (perform (make-instance 'load-source-op) c))

#+asdf2.27
(defsystem :asdf/defsystem
  :licence "MIT"
  :description "The defsystem part of ASDF"
  :description "Generate asdf.lisp based on this and monolithic-concatenate-source-op"
  :defsystem-depends-on (:asdf)
  :version (:read-file-form "version.lisp-expr")
  :class :bundle-system
  :build-operation monolithic-concatenate-source-op
  :bundle-pathname "build/asdf"
  :serial t
  :around-compile call-without-redefinition-warnings ;; be the same as asdf-driver
  :depends-on (:asdf/header :asdf-driver)
  :components
  ((:file "upgrade")
   (:file "component")
   (:file "system" :depends-on ("component"))
   (:file "find-system" :depends-on ("system"))
   (:file "find-component" :depends-on ("find-system"))
   (:file "operation")
   (:file "action" :depends-on ("find-component" "operation"))
   (:file "lisp-action" :depends-on ("action"))
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

#+asdf2.27
(defsystem :asdf/header
  :components
  ((:static-file "header.lisp")))
