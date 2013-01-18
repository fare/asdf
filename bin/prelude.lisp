(setf *load-verbose* nil *load-print* nil
      *compile-verbose* nil *compile-print* nil)

(format t "Loading your implementation's ASDF... ~%")
(require :asdf)
(in-package :asdf)
#-asdf2 (error "Not ASDF2, you lose!")
(format t "Initializing the source registry... ~%")
(initialize-source-registry)
(format t "Upgrading to the latest ASDF... ~%")
(upgrade-asdf)
(format t "At ASDF ~A.~%" (asdf-version))
(format t "Now loading some dependencies... ~%")
(load-systems :cl-ppcre :fare-utils :inferior-shell)

(format t "There we are!~%")
(restore-image)

