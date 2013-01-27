;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in asdf/driver

(asdf/package:define-package :asdf/driver
  (:nicknames :d :asdf-driver :asdf-utils)
  (:use :asdf/common-lisp :asdf/package :asdf/utility
   :asdf/pathname :asdf/stream :asdf/os :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration :asdf/backward-driver)
  (:reexport
   ;; NB: excluding asdf/common-lisp
   ;; which include all of CL with compatibility modifications on select platforms.
   :asdf/package :asdf/utility
   :asdf/pathname :asdf/stream :asdf/os :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration :asdf/backward-driver))
