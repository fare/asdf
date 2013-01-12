;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2012 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)

(defsystem :asdf
  :author ("Daniel Barlow")
  :maintainer ("Francois-Rene Rideau")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "2.26.81" ;; to be automatically updated by bin/bump-revision
  :depends-on ()
  :components ((:module "build" :components ((:file "asdf"))))
  :in-order-to (#+asdf2.27 (compile-op (monolithic-load-concatenated-source-op generate-asdf))))

#-asdf2.27
(defmethod perform :before
    ((o compile-op)
     (c (eql (first (module-components
                     (first (module-components (find-system :asdf))))))))
  (declare (ignorable o))
  (perform (make-instance 'load-source-op) c))

#+(and clisp (not asdf2.27))
(rename-package :asdf :asdf-utilities)
