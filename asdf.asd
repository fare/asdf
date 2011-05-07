;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2011 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :asdf
  :author ("Daniel Barlow")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "2.015" ;; to be automatically updated by bin/bump-revision
  :depends-on ()
  :components
  ((:file "asdf")
   #+ecl (:file "asdf-ecl" :depends-on ("asdf"))))
