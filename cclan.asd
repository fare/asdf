;;; -*- Lisp -*- 
(defpackage :cclan-system (:use #:cl #:asdf))
(in-package :cclan-system)

(defsystem cclan
    :version "0.0"
    #+common-lisp-controller :pathname  #+common-lisp-controller "cl-library:cclan;"
    :components ((:file "cclan-package")
		 (:file "cclan" :depends-on ("cclan-package"))))
