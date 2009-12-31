;;; -*- Lisp -*-
(defpackage :cclan-system (:use #:cl #:asdf))
(in-package :cclan-system)

(defsystem cclan
    :version "0.1"
    :components ((:file "cclan-package")
                 (:file "cclan" :depends-on ("cclan-package"))))
