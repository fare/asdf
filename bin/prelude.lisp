(setf *load-verbose* nil *load-print* nil
      *compile-verbose* nil *compile-print* nil)

(format t "Loading your implementation's ASDF... ~%")
(require :asdf)
(format t "~:[No ~;~]ASDF~:*~@[ ~A~] was provided~%"
  (when (find-package :asdf)
    (or (symbol-value (or (find-symbol (string :*asdf-version*) :asdf)
                          (find-symbol (string :*asdf-revision*) :asdf)))
        (string :1.x))))
#-asdf2 (load (merge-pathnames "../build/asdf.lisp" *load-pathname*))
#-asdf2 (error "Not ASDF2, you lose!")

(in-package :asdf)
(defparameter *provided-version* (asdf-version))
(format t "Initializing the source registry... ~%")
(initialize-source-registry)
(format t "Upgrading to the latest ASDF... ~%")
(load-system :asdf)
(let ((ver (asdf-version)))
  (if (equal ver *provided-version*)
      (format t "Congratulations to your implementation for being up to date!~%")
      (format t "Upgraded to ASDF ~A~%" ver)))
(format t "Now loading some dependencies... ~%")
(load-systems :cl-ppcre :fare-utils :inferior-shell)

(format t "There we are!~%")
(restore-image)

