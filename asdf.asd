;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2010 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :asdf
  :author ("Daniel Barlow")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :depends-on ()
  :components
  ((:file "asdf")
   #+ecl (:file "asdf-ecl" :depends-on ("asdf"))))
