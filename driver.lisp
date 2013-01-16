;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in asdf/driver

(asdf/package:define-package :asdf/driver
  (:nicknames :d :asdf-driver)
  (:use :common-lisp
   :asdf/package :asdf/compatibility :asdf/utility
   :asdf/pathname :asdf/stream :asdf/os :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration)
  (:reexport
   :asdf/package :asdf/compatibility :asdf/utility
   :asdf/pathname :asdf/stream :asdf/os :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration))
