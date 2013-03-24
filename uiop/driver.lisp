;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in asdf/driver

(uiop/package:define-package :uiop/driver
  (:nicknames :uiop :asdf/driver :asdf-driver :asdf-utils)
  (:use :uiop/common-lisp :uiop/package :uiop/utility
    :uiop/os :uiop/pathname :uiop/stream :uiop/filesystem :uiop/image
   :uiop/run-program :uiop/lisp-build
   :uiop/configuration :uiop/backward-driver)
  (:reexport
   ;; NB: excluding asdf/common-lisp
   ;; which include all of CL with compatibility modifications on select platforms.
   :uiop/package :uiop/utility
   :uiop/os :uiop/pathname :uiop/stream :uiop/filesystem :uiop/image
   :uiop/run-program :uiop/lisp-build
   :uiop/configuration :uiop/backward-driver))
