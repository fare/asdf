;;; -*- Mode: common-lisp; Syntax: Common-Lisp; -*-

;; copyright (c) 2006-2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: db-agraph-preflight.asd,v 1.1.2.1 2009/01/16 23:58:53 gwking Exp $

;; -*- mode: common-lisp -*-

(in-package #:common-lisp-user)

(defpackage #:test-nested-components.system
            (:use #:common-lisp #:asdf))

(in-package #:test-nested-components.system)

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
