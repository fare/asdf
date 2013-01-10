;;;; -------------------------------------------------------------------------
;;;; Handle upgrade as forward- and backward-compatibly as possible
;; See https://bugs.launchpad.net/asdf/+bug/485687

(asdf/package:define-package :asdf/upgrade
  (:recycle :asdf/upgrade :asdf)
  (:use :common-lisp :asdf/package :asdf/implementation :asdf/utility)
  (:export
   #:upgrade-asdf #:asdf-upgrade-error #:with-upgrade
   #:*post-upgrade-cleanup-hook* #:*post-upgrade-restart-hook*
   #:asdf-version #:*upgraded-p*))
(in-package :asdf/upgrade)

;;; Special magic to detect if this is an upgrade

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *asdf-version* nil)
  (defvar *upgraded-p* nil)
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. The script bin/bump-version
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of this file.
         ;; "2.345" would be an official release
         ;; "2.345.6" would be a development version in the official upstream
         ;; "2.345.0.7" would be your seventh local modification of official release 2.345
         ;; "2.345.6.7" would be your seventh local modification of development version 2.345.6
         (asdf-version "2.26.69")
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


;;;; User interface

(defmacro with-upgrade ((&key (upgraded-p '*upgraded-p*) when) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (when (and ,upgraded-p ,@(when when `(,when)))
       (handler-bind ((style-warning #'muffle-warning))
         (eval '(progn ,@body))))))

(defun* asdf-version ()
  "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.:
(ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"2.345.67\")."
  *asdf-version*)

(defun* asdf-upgrade-error ()
  ;; Important notice for whom it concerns. The crux of the matter is that
  ;; TRAVERSE can be completely refactored, and so after the find-system returns, it's too late.
  (error "When a system transitively depends on ASDF, it must :defsystem-depends-on (:asdf)~%~
          Otherwise, when you upgrade ASDF, you must do it before you operate on any system.~%"))


;;; Self-upgrade functions

(defvar *post-upgrade-cleanup-hook* ())
(defvar *post-upgrade-restart-hook* ())

(defun* post-upgrade-cleanup (old-version)
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
  (let ((version (asdf-version)))
    (handler-bind (((or style-warning warning) #'muffle-warning))
      (funcall (find-symbol* 'load-system :asdf) :asdf :verbose nil))
    (post-upgrade-cleanup version)))

