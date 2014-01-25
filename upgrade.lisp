;;;; -------------------------------------------------------------------------
;;;; Handle upgrade as forward- and backward-compatibly as possible
;; See https://bugs.launchpad.net/asdf/+bug/485687

(asdf/package:define-package :asdf/upgrade
  (:recycle :asdf/upgrade :asdf)
  (:use :uiop/common-lisp :uiop)
  (:export
   #:asdf-version #:*previous-asdf-versions* #:*asdf-version*
   #:asdf-message #:*verbose-out*
   #:upgrading-p #:when-upgrading #:upgrade-asdf #:asdf-upgrade-error #:defparameter*
   #:*post-upgrade-cleanup-hook* #:*post-upgrade-restart-hook* #:cleanup-upgraded-asdf
   ;; There will be no symbol left behind!
   #:intern*)
  (:import-from :asdf/package #:intern* #:find-symbol*))
(in-package :asdf/upgrade)

;;; Special magic to detect if this is an upgrade

(with-upgradability ()
  (defun asdf-version ()
    "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.: (ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"3.4.5.67\")."
    (when (find-package :asdf)
      (or (symbol-value (find-symbol (string :*asdf-version*) :asdf))
          (let* ((revsym (find-symbol (string :*asdf-revision*) :asdf))
                 (rev (and revsym (boundp revsym) (symbol-value revsym))))
            (etypecase rev
              (string rev)
              (cons (format nil "~{~D~^.~}" rev))
              (null "1.0"))))))
  ;; Important: define *p-a-v* /before/ *a-v* so that it initializes correctly.
  (defvar *previous-asdf-versions* (if-let (previous (asdf-version)) (list previous)))
  (defvar *asdf-version* nil)
  ;; We need to clear systems from versions yet older than the below:
  (defparameter *oldest-forward-compatible-asdf-version* "2.33") ;; 2.32.13 renames a slot in component.
  (defmacro defparameter* (var value &optional docstring)
    (let* ((name (string-trim "*" var))
           (valfun (intern (format nil "%~A-~A-~A" :compute name :value)))
           (clearfun (intern (format nil "%~A-~A" :clear name))))
      `(progn
         (defun ,valfun () ,value)
         (defvar ,var (,valfun) ,@(ensure-list docstring))
         (defun ,clearfun () (setf ,var (,valfun)))
         (register-hook-function '*post-upgrade-cleanup-hook* ',clearfun))))
  (defvar *verbose-out* nil)
  (defun asdf-message (format-string &rest format-args)
    (when *verbose-out* (apply 'format *verbose-out* format-string format-args)))
  (defvar *post-upgrade-cleanup-hook* ())
  (defvar *post-upgrade-restart-hook* ())
  (defun upgrading-p ()
    (and *previous-asdf-versions* (not (equal *asdf-version* (first *previous-asdf-versions*)))))
  (defmacro when-upgrading ((&key (upgrading-p '(upgrading-p)) when) &body body)
    `(with-upgradability ()
       (when (and ,upgrading-p ,@(when when `(,when)))
         (handler-bind ((style-warning #'muffle-warning))
           (eval '(progn ,@body))))))
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. make bump-version v=3.4.5.67.8
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of asdf.lisp.
         ;; "3.4" would be the general branch for major version 3, minor version 4.
         ;; "3.4.5" would be an official release in the 3.4 branch.
         ;; "3.4.5.67" would be a development version in the official branch, on top of 3.4.5.
         ;; "3.4.5.0.8" would be your eighth local modification of official release 3.4.5
         ;; "3.4.5.67.8" would be your eighth local modification of development version 3.4.5.67
         (asdf-version "3.1.0.54")
         (existing-version (asdf-version)))
    (setf *asdf-version* asdf-version)
    (when (and existing-version (not (equal asdf-version existing-version)))
      (push existing-version *previous-asdf-versions*)
      (when (or *verbose-out* *load-verbose*)
        (format (or *verbose-out* *trace-output*)
                (compatfmt "~&~@<; ~@;Upgrading ASDF ~@[from version ~A ~]to version ~A~@:>~%")
                existing-version asdf-version)))))

(when-upgrading ()
  (let ((redefined-functions ;; gf signature and/or semantics changed incompatibly. Oops.
          '(#:component-relative-pathname #:component-parent-pathname ;; component
            #:source-file-type
            #:find-system #:system-source-file #:system-relative-pathname ;; system
             #:find-component ;; find-component
             #:explain #:perform #:perform-with-restarts #:input-files #:output-files ;; action
             #:component-depends-on #:operation-done-p #:component-depends-on
             #:traverse ;; backward-interface
             #:operate  ;; operate
             #:parse-component-form ;; defsystem
             #:apply-output-translations ;; output-translations
             #:process-output-translations-directive
             #:inherit-source-registry #:process-source-registry ;; source-registry
             #:process-source-registry-directive
             #:trivial-system-p ;; bundle
             ;; NB: it's too late to do anything about uiop functions!
             ))
         (uninterned-symbols
           '(#:*asdf-revision* #:around #:asdf-method-combination
             #:split #:make-collector #:do-dep #:do-one-dep
             #:component-self-dependencies
             #:resolve-relative-location-component #:resolve-absolute-location-component
             #:output-files-for-system-and-operation))) ; obsolete ASDF-BINARY-LOCATION function
    (declare (ignorable redefined-functions uninterned-symbols))
    (loop :for name :in (append redefined-functions)
          :for sym = (find-symbol* name :asdf nil) :do
            (when sym
              ;; On CLISP we seem to be unable to fmakunbound and define a function in the same fasl. Sigh.
              #-clisp (fmakunbound sym)))
    (loop :with asdf = (find-package :asdf)
          :for name :in uninterned-symbols
          :for sym = (find-symbol* name :asdf nil)
          :for base-pkg = (and sym (symbol-package sym)) :do
            (when sym
              (cond
                ((or (eq base-pkg asdf) (not base-pkg))
                 (unintern* sym asdf)
                 (intern* sym asdf))
                (t
                 (unintern* sym base-pkg)
                 (let ((new (intern* sym base-pkg)))
                   (shadowing-import new asdf))))))))


;;; Self-upgrade functions

(with-upgradability ()
  (defun asdf-upgrade-error ()
    ;; Important notice for whom it concerns. The crux of the matter is that
    ;; TRAVERSE can be completely refactored, and so after the find-system returns, it's too late.
    (error "When a system transitively depends on ASDF, it must :defsystem-depends-on (:asdf)~%~
          Otherwise, when you upgrade from ASDF 2, you must do it before you operate on any system.~%"))

  (defun cleanup-upgraded-asdf (&optional (old-version (first *previous-asdf-versions*)))
    (let ((new-version (asdf-version)))
      (unless (equal old-version new-version)
        (push new-version *previous-asdf-versions*)
        (when old-version
          (if (version<= new-version old-version)
              (error (compatfmt "~&~@<; ~@;Downgraded ASDF from version ~A to version ~A~@:>~%")
                     old-version new-version)
              (asdf-message (compatfmt "~&~@<; ~@;Upgraded ASDF from version ~A to version ~A~@:>~%")
                            old-version new-version))
          ;; In case the previous version was too old to be forward-compatible, clear systems.
          ;; TODO: if needed, we may have to define a separate hook to run
          ;; in case of forward-compatible upgrade.
          ;; Or to move the tests forward-compatibility test inside each hook function?
          (unless (version<= *oldest-forward-compatible-asdf-version* old-version)
            (call-functions (reverse *post-upgrade-cleanup-hook*)))
          t))))

  (defun upgrade-asdf ()
    "Try to upgrade of ASDF. If a different version was used, return T.
   We need do that before we operate on anything that may possibly depend on ASDF."
    (let ((*load-print* nil)
          (*compile-print* nil))
      (handler-bind (((or style-warning) #'muffle-warning))
        (symbol-call :asdf :load-system :asdf :verbose nil))))

  (register-hook-function '*post-upgrade-cleanup-hook* 'upgrade-configuration))

