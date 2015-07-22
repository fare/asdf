;;;; -------------------------------------------------------------------------
;;; Internal hacks for backward-compatibility

(uiop/package:define-package :asdf/backward-internals
  (:recycle :asdf/backward-internals :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export ;; for internal use
   #:make-sub-operation
   #:load-sysdef #:make-temporary-package))
(in-package :asdf/backward-internals)

(when-upgrading (:when (fboundp 'make-sub-operation))
  (defun make-sub-operation (c o dep-c dep-o)
    (declare (ignore c o dep-c dep-o)) (asdf-upgrade-error)))

;;;; load-sysdef
(with-upgradability ()
  (defun load-sysdef (name pathname)
    (load-asd pathname :name name))

  (defun make-temporary-package ()
    ;; For loading a .asd file, we don't make a temporary package anymore,
    ;; but use ASDF-USER. I'd like to have this function do this,
    ;; but since whoever uses it is likely to delete-package the result afterwards,
    ;; this would be a bad idea, so preserve the old behavior.
    (make-package (fresh-package-name :prefix :asdf :index 0) :use '(:cl :asdf))))

