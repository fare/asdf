
(defpackage :cclan-system (:use #:cl #:asdf))

(defsystem 'cclan
	:components
	(:file "cclan-package")
	(:file "cclan" :depends-on ("cclan-package")))