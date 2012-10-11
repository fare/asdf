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
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :version "2.25.3" ;; to be automatically updated by bin/bump-revision
  :depends-on ()
  :components
  ((:file "asdf")
   #+ecl (:file "asdf-ecl" :depends-on ("asdf"))))

;; The method below ensures that before we compile asdf, we load it as source.
;; This ensures that when we compile asdf, it won't remove symbols and packages
;; in the back of the compiling asdf, which then finds itself incapable of
;; perform'ing the load-op'ing of the newly compiled asdf fasl because
;; perform has been undefined during the initial package-frobbing eval-when code,
;; but not redefined yet by loading the code rather than merely compiling it.
;; We could use (:file "asdf" :do-first ((compile-op (load-source-op "asdf"))))
;; but it's only supported since ASDF 2.016.3. What's below should be more compatible.
;; We can't use find-component, because it's not compatible with old versions of ASDF 1.x

(defmethod perform :before ((operation compile-op)
			    (c (eql (first (module-components (find-system :asdf))))))
  (perform (make-instance 'load-source-op) c))
