;;; -*- Lisp -*- 
(defpackage :cclan-system (:use #:cl #:asdf))

(defsystem cclan
    :version "0.0"
    :components ((:file "cclan-package")
		 (:file "cclan" :depends-on ("cclan-package"))))