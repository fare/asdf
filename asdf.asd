;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2013 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

#+asdf2.27
(defsystem :asdf/header
  ;; Note that it's polite to sort the defsystem forms in dependency order,
  ;; and compulsory to sort them in defsystem-depends-on order.
  :version (:read-file-form "version.lisp-expr")
  :around-compile call-without-redefinition-warnings ;; we need be the same as asdf-driver
  :components
  ((:file "header")))

#+asdf2.27
(defsystem :asdf/defsystem
  :licence "MIT"
  :description "The defsystem part of ASDF"
  :long-description "Another System Definition Facility, the portable defsystem for Common Lisp"
  :version (:read-file-form "version.lisp-expr")
  :build-operation monolithic-concatenate-source-op
  :build-pathname "build/asdf" ;; our target
  :around-compile call-without-redefinition-warnings ;; we need be the same as asdf-driver
  :depends-on (:asdf/header :asdf-driver)
  :components
  ((:file "upgrade")
   (:file "component" :depends-on ("upgrade"))
   (:file "system" :depends-on ("component"))
   (:file "stamp-cache" :depends-on ("upgrade"))
   (:file "find-system" :depends-on ("system" "stamp-cache"))
   (:file "find-component" :depends-on ("find-system"))
   (:file "operation" :depends-on ("upgrade"))
   (:file "action" :depends-on ("find-component" "operation"))
   (:file "lisp-action" :depends-on ("action"))
   (:file "plan" :depends-on ("lisp-action" "stamp-cache"))
   (:file "operate" :depends-on ("plan"))
   (:file "output-translations" :depends-on ("operate"))
   (:file "source-registry" :depends-on ("find-system"))
   (:file "backward-internals" :depends-on ("lisp-action" "operate"))
   (:file "defsystem" :depends-on ("backward-internals" "stamp-cache"))
   (:file "bundle" :depends-on ("lisp-action"))
   (:file "concatenate-source" :depends-on ("bundle"))
   (:file "backward-interface" :depends-on ("operate" "output-translations"))
   (:file "interface" :depends-on
          ("defsystem" "concatenate-source"
           "backward-interface" "backward-internals"
           "output-translations" "source-registry"))
   (:file "footer" :depends-on ("interface"))))

(defsystem :asdf
  :author ("Daniel Barlow")
  :maintainer ("Francois-Rene Rideau")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "2.26.148" ;; to be automatically updated by make bump-version
  :depends-on ()
  :components
  ((:module "build"
    :components
    (#-gcl2.6
     (:file "asdf"
      #-asdf2.27 :do-first #-asdf2.27 ((compile-op (load-source-op "asdf")))
      ))))
  :in-order-to
  (#+asdf2.27 (compile-op (monolithic-load-concatenated-source-op asdf/defsystem))))
