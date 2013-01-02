;;; -*- Mode: common-lisp; Syntax: Common-Lisp; -*-

(in-package :common-lisp-user)

(defpackage #:test-nested-components.system
            (:use #:common-lisp #:asdf))

(in-package :test-nested-components.system)

(defsystem test-nested-components-a
  :components
  ((:module "nested-components"
            :pathname ""
            :components ((:file "test-nested-1")))))

(defsystem test-nested-components-b
  :pathname ""
  :components
  ((:file "test-nested-1")))

(defsystem db-agraph-preflight
  :components
  ((:module "preflight-checks"
            :components ((:file "preflight")))))

(defsystem db-agraph-preflight-2
  :pathname "preflight-checks"
  :components
  ((:file "preflight")))

#|
newer traverse always fails
older traverse fails when db-agraph-preflight is evaluated, ok
  when loaded or compiled
|#
