;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in asdf/driver

(asdf/package:define-package :asdf/driver
  (:nicknames :asdf-driver :asdf-utils)
  (:use :asdf/common-lisp :asdf/package :asdf/utility
    :asdf/os :asdf/pathname :asdf/stream :asdf/filesystem :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration :asdf/backward-driver)
  (:reexport
   ;; NB: excluding asdf/common-lisp
   ;; which include all of CL with compatibility modifications on select platforms.
   :asdf/package :asdf/utility
    :asdf/os :asdf/pathname :asdf/stream :asdf/filesystem :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration :asdf/backward-driver))
