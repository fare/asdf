;;; -*- Mode: common-lisp; Syntax: Common-Lisp; -*-

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
