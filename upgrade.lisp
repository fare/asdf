;;;; -------------------------------------------------------------------------
;;;; Handle upgrade as forward- and backward-compatibly as possible
;; See https://bugs.launchpad.net/asdf/+bug/485687

(asdf/package:define-package :asdf/upgrade
  (:recycle :asdf/upgrade :asdf)
  (:use :common-lisp :asdf/package :asdf/compatibility :asdf/utility)
  (:export
   #:upgrade-asdf #:asdf-upgrade-error #:when-upgrade
   #:*asdf-upgrade-already-attempted*
   #:*post-upgrade-cleanup-hook* #:*post-upgrade-restart-hook* #:cleanup-upgraded-asdf
   #:asdf-version #:*upgraded-p*
   #:asdf-message #:*asdf-verbose* #:*verbose-out*))
(in-package :asdf/upgrade)

;;; Special magic to detect if this is an upgrade

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *asdf-version* nil)
  (defvar *upgraded-p* nil)
  (defvar *asdf-verbose* nil) ; was t from 2.000 to 2.014.12.
  (defvar *verbose-out* nil)
  (defun* asdf-message (format-string &rest format-args)
    (apply 'format *verbose-out* format-string format-args)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. The script bin/bump-version
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of this file.
         ;; "2.345" would be an official release
         ;; "2.345.6" would be a development version in the official upstream
         ;; "2.345.0.7" would be your seventh local modification of official release 2.345
         ;; "2.345.6.7" would be your seventh local modification of development version 2.345.6
         (asdf-version "2.26.84")
         (existing-asdf (find-class (find-symbol* :component :asdf nil) nil))
         (existing-version *asdf-version*)
         (already-there (equal asdf-version existing-version)))
    (unless (and existing-asdf already-there)
      (when existing-asdf
        (asdf-message "~&; Upgrading ASDF ~@[from version ~A ~]to version ~A~%"
                      existing-version asdf-version))
      (unless already-there
        (push existing-version *upgraded-p*))
      (setf *asdf-version* asdf-version))))


;;; Upgrade interface

(defun* asdf-upgrade-error ()
  ;; Important notice for whom it concerns. The crux of the matter is that
  ;; TRAVERSE can be completely refactored, and so after the find-system returns, it's too late.
  (error "When a system transitively depends on ASDF, it must :defsystem-depends-on (:asdf)~%~
          Otherwise, when you upgrade ASDF, you must do it before you operate on any system.~%"))

(defmacro when-upgrade ((&key (upgraded-p '*upgraded-p*) when) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (when (and ,upgraded-p ,@(when when `(,when)))
       (handler-bind ((style-warning #'muffle-warning))
         (eval '(progn ,@body))))))

(defun* asdf-version ()
  "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.:
(ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"2.345.67\")."
  *asdf-version*)


;;; Self-upgrade functions

(defvar *asdf-upgrade-already-attempted* nil)

(defvar *post-upgrade-cleanup-hook* ())
(defvar *post-upgrade-restart-hook* ())

(defun* cleanup-upgraded-asdf (old-version)
  (let ((new-version (asdf-version)))
    (unless (equal old-version new-version)
      (cond
        ((version-compatible-p new-version old-version)
         (asdf-message (compatfmt "~&~@<; ~@;Upgraded ASDF from version ~A to version ~A~@:>~%")
                       old-version new-version))
        ((version-compatible-p old-version new-version)
         (warn (compatfmt "~&~@<; ~@;Downgraded ASDF from version ~A to version ~A~@:>~%")
               old-version new-version))
        (t
         (asdf-message (compatfmt "~&~@<; ~@;Changed ASDF from version ~A to incompatible version ~A~@:>~%")
                       old-version new-version)))
      (dolist (h (reverse *post-upgrade-cleanup-hook*))
        (funcall (ensure-function h)))
      (dolist (h *post-upgrade-restart-hook*)
        (funcall (ensure-function h)))
      t)))

(defun* upgrade-asdf ()
  "Try to upgrade of ASDF. If a different version was used, return T.
   We need do that before we operate on anything that depends on ASDF."
  (unless *asdf-upgrade-already-attempted*
    (setf *asdf-upgrade-already-attempted* t)
    (let ((version (asdf-version)))
      (handler-bind (((or style-warning warning) #'muffle-warning))
        (symbol-call :asdf :load-system :asdf :verbose nil))
      (cleanup-upgraded-asdf version))))
