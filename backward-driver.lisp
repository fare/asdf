;;;; -------------------------------------------------------------------------
;;; Hacks for backward-compatibility of the driver

(asdf/package:define-package :asdf/backward-driver
  (:recycle :asdf/backward-driver :asdf)
  (:use :common-lisp
   :asdf/package :asdf/compatibility :asdf/utility
   :asdf/pathname :asdf/stream :asdf/os :asdf/image
   :asdf/run-program :asdf/lisp-build
   :asdf/configuration)
  (:export
   #:coerce-pathname #:component-name-to-pathname-components))
(in-package :asdf/backward-driver)

;;;; Backward compatibility with various pathname functions.

(defun* coerce-pathname (name &key type defaults)
  ;; For backward-compatibility only, for people using internals
  ;; Reported users in quicklisp: hu.dwim.asdf, asdf-utils, xcvb
  ;; Will be removed after 2014-01-16.
  ;;(warn "Please don't use ASDF::COERCE-PATHNAME. Use ASDF/PATHNAME:PARSE-UNIX-NAMESTRING.")
  (parse-unix-namestring name :type type :defaults defaults))

(defun* component-name-to-pathname-components (unix-style-namestring
                                               &key force-directory force-relative)
  ;; Will be removed after 2014-01-16.
  ;; (warn "Please don't use ASDF::COMPONENT-NAME-TO-PATHNAME-COMPONENTS, use SPLIT-UNIX-NAMESTRING-DIRECTORY-COMPONENTS")
  (multiple-value-bind (relabs path filename file-only)
      (split-unix-namestring-directory-components
       unix-style-namestring :want-directory force-directory)
    (declare (ignore file-only))
    (when (and force-relative (not (eq relabs :relative)))
      (error (compatfmt "~@<Absolute pathname designator not allowed: ~3i~_~S~@:>")
             unix-style-namestring))
    (values relabs path filename)))

