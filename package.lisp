;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.
;;
;; See https://bugs.launchpad.net/asdf/+bug/485687
;;
;; CAUTION: we must handle the first few packages specially for hot-upgrade.
;; asdf/package will be frozen as of 2.27
;; to forever export the same exact symbols.
;; Any other symbol must be import-from'ed
;; and reexported in a different package
;; (alternatively the package may be dropped & replaced by one with a new name).

(defpackage :asdf/package
  (:use :common-lisp)
  (:export
   #:find-symbol* #:present-symbol-p #:present-symbols
   #:intern* #:remove-symbol #:unlink-package #:ensure-package
   #:define-package))

(in-package :asdf/package)

;;;; General purpose package utilities

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun find-symbol* (name package-name &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unless CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (let ((package (find-package package-name)))
      (if package
          (let ((symbol (find-symbol (string name) package)))
            (or symbol
                (when error
                  (error "There is no symbol ~A in package ~A" name package-name))))
          (when error
            (error "There is no package ~A" package-name)))))
  (defun intern* (name package)
    (intern (string name) package))
  (defun remove-symbol (symbol package)
    (let ((sym (find-symbol* symbol package nil)))
      (when sym
        #-cormanlisp (unexport sym package)
        (unintern sym package)
        sym)))
  (defun present-symbol-p (symbol package)
    (member (nth-value 1 (find-symbol* symbol package nil)) '(:internal :external)))
  (defun present-symbols (package)
    ;; #-genera (loop :for s :being :the :present-symbols :in package :collect s) #+genera
    (let (l) (do-symbols (s package) (when (present-symbol-p s package) (push s l))) (reverse l))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun soft-upgrade-p (upgrade)
    (ecase upgrade ((:soft nil) t) (:hard nil)))
  (defun check-packages-exist (package-names)
    (remove-duplicates
     (loop :for n :in package-names
           :for p = (find-package n)
           :when p :collect p :else :do (error "Package ~A does not exist" n))
     :from-end t))
  (defun ensure-package-unused (package)
    (loop :for p :in (package-used-by-list package) :do
      (unuse-package package p)))
  (defun ensure-package-deleted (package) ;; &key upgrade
    (let ((p (find-package package)))
      (when p
        ;;(unless (soft-upgrade-p upgrade) (ensure-package-unintern p (present-symbols u)))
        (ensure-package-unused p)
        (delete-package package))))
  (defun ensure-package-fmakunbound (package symbols)
    (loop :for name :in symbols
          :for sym = (find-symbol* name package nil)
          :when sym :do (fmakunbound sym)))
  (defun ensure-package-fmakunbound-setf (package symbols)
    (loop :for name :in symbols
          :for sym = (find-symbol* name package nil)
          :when sym :do #-gcl (fmakunbound `(setf ,sym))))
  (defun ensure-package-unintern (package unintern &key table (users (list-all-packages)))
    (loop :for u :in unintern
          :for name = (string u)
          :for removed = (remove-symbol name package) :do
          (when removed
            (loop :for p :in users :do
              (when (eq removed (find-symbol name p))
                (unintern removed p)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun recycle-symbol (name recycle)
    (loop :for r :in recycle
          :for s = (find-symbol* name r nil)
          :when s :do (values s r)))
  (defun ensure-package (name &key
                                upgrade
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern fmakunbound fmakunbound-setf)
    (let* ((nicknames (mapcar #'string nicknames))
           (use (check-packages-exist use))
           (shadow (mapcar #'string shadow))
           (shadowing-import-from (loop :for sif :in shadowing-import-from
                                        :collect (mapcar #'string sif)))
           (import-from (loop :for if :in import-from
                              :collect (mapcar #'string if)))
           (export (mapcar #'string export))
           (recycle (remove nil (mapcar #'find-package recycle)))
           (mix (check-packages-exist mix))
           (shadowed (make-hash-table :test 'equal))
           (inherited (make-hash-table :test 'equal))
           (imported (make-hash-table :test 'equal))
           (exported (make-hash-table :test 'equal))
           (previous (remove-duplicates
                      (mapcar #'find-package (cons name nicknames))
                      :from-end t))
           (discarded (cdr previous))
           (package (or (first previous) (make-package name :nicknames nicknames))))
      (setf (documentation package t) documentation)
      (assert (soft-upgrade-p upgrade))
      (ensure-package-unused package)
      (map () #'ensure-package-deleted discarded)
      (rename-package package name nicknames)
      (dolist (name unintern) (remove-symbol name package))
      ;;; Compute the desired state of the package
      (loop :for sym :in shadow :for name = (string sym) :do
        (setf (gethash name shadowed) t)
        (multiple-value-bind (recycled previous) (recycle-symbol name recycle)
          (cond
            ((or (not previous) (not (member (symbol-package recycle) recycle)))
             (ecase (nth-value 1 (find-symbol* name package nil))
               ((nil :inherited) (shadow name package))
               ((:internal :external) (shadowing-import (make-symbol name) package))))
            ((eq previous package) (shadow recycled package))
            (t (unintern recycled previous) (shadowing-import recycled package)))))
      (labels
          ((ensure-shadowing-import (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (cond
                 ((gethash name shadowed)
                  (unless (eq i (gethash name imported))
                    (error "Conflicting shadowings for ~A" name)))
                 (t
                    (setf (gethash name shadowed) t)
                    (setf (gethash name imported) i)
                    (shadowing-import package)))))
           (ensure-import (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (multiple-value-bind (ii ip) (gethash name imported)
                 (cond
                   (ip
                    (unless (eq i ii)
                      (error "Can't import ~S from both ~S and ~S"
                             name (package-name (symbol-package ii)) (package-name p))))
                   ((gethash name shadowed)
                    (error "Can't both shadow ~S and import it from ~S" name (package-name p)))
                   (t
                    (multiple-value-bind (x xp) (find-symbol name package)
                      (when (and xp (not (eq i x)))
                        (unintern x package))
                      (import i package)))))))
           (ensure-mix (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (unless (or (gethash name shadowed) (nth-value 1 (gethash name imported)))
                 (multiple-value-bind (in inp) (gethash name inherited)
                   (cond
                     ((eq i in))
                     (inp
                      (remhash name inherited)
                      (ensure-shadowing-import name (symbol-package in)))
                     (t
                      (ensure-inherited sym p)))))))
           (ensure-inherited (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (multiple-value-bind (in inp) (gethash name inherited)
                 (multiple-value-bind (im imp) (gethash name imported)
                   (cond
                     (inp
                      (unless (eq i in)
                        (error "Can't inherit ~S from ~S, it is inherited from ~S"
                               name (package-name p) (package-name (symbol-package in)))))
                     (imp
                      (unless (eq i im)
                        (error "Can't inherit ~S from ~S, it is imported from ~S"
                               name (package-name p) (package-name (symbol-package im)))))
                     ((gethash name shadowed)
                      (error "Can't inherit ~S from ~S, it is shadowed" name (package-name p)))
                     (t
                      (multiple-value-bind (x xp) (find-symbol name package)
                        (when xp
                          (unintern x package)))))))))
           (ensure-registered-symbol (sym)
             (let ((name (string sym)))
               (unless (or (gethash name shadowed)
                           (nth-value 1 (gethash name imported))
                           (nth-value 1 (gethash name inherited)))
                 (multiple-value-bind (recycled previous) (recycle-symbol name recycle)
                   (cond
                       ((or (not previous) (not (member (symbol-package recycle) recycle)))
                        (unintern sym package))
                       ((eq previous package))
                       (t (unintern recycled previous) (import recycled package))))))))
        (loop :for (p . syms) :in shadowing-import-from :do
          (dolist (sym syms) (ensure-shadowing-import sym p)))
        (loop :for p :in mix :do
          (do-external-symbols (sym p) (ensure-mix sym p)))
        (loop :for (p . syms) :in import-from :do
          (dolist (sym syms) (ensure-import sym p)))
        (loop :for p :in use :do
          (do-external-symbols (sym p) (ensure-inherited sym p)))
        (loop :for p :in use :do (use-package p package))
        (dolist (sym intern) (intern* sym package))
        (do-symbols (sym package)
          (ensure-registered-symbol sym))
        (loop :for p :in reexport :do
        (do-external-symbols (sym p)
          (let ((name (string sym)))
            (export (find-symbol* name package) package) (setf (gethash name exported) t))))
        (dolist (sym export) (export sym package) (setf (gethash sym exported) t))
        (do-external-symbols (sym package) (unless (gethash sym exported) (unexport sym)))
        ;; do away with packages with conflicting (nick)names
        ;; note from ASDF 2.26: ECL might not be liking an early fmakunbound (below #-ecl'ed)
        (ensure-package-fmakunbound package fmakunbound)
        (ensure-package-fmakunbound-setf package fmakunbound-setf)
        package))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-define-package-clauses (package clauses)
    (loop
      :with use-p = nil :with recycle-p = nil
      :with documentation = nil :with upgrade = nil
      :for (kw . args) :in clauses
      :when (eq kw :nicknames) :append args :into nicknames :else
      :when (eq kw :documentation)
        :do (cond
              (documentation (error "define-package: can't define documentation twice"))
              ((or (atom args) (cdr args)) (error "define-package: bad documentation"))
              (t (setf documentation (car args)))) :else
      :when (eq kw :use) :append args :into use :and :do (setf use-p t) :else
      :when (eq kw :shadow) :append args :into shadow :else
      :when (eq kw :shadowing-import-from) :collect args :into shadowing-import-from :else
      :when (eq kw :import-from) :collect args :into import-from :else
      :when (eq kw :export) :append args :into export :else
      :when (eq kw :intern) :append args :into intern :else
      :when (eq kw :recycle) :append args :into recycle :and :do (setf recycle-p t) :else
      :when (eq kw :mix) :append args :into mix :else
      :when (eq kw :reexport) :append args :into reexport :else
      :when (eq kw :unintern) :append args :into unintern :else
      :when (eq kw :fmakunbound) :append args :into fmakunbound :else
      :when (eq kw :fmakunbound-setf) :append args :into fmakunbound-setf :else
      :when (eq kw :upgrade)
        :do (unless (and (consp args) (null (cdr args)) (member (car args) '(:soft :hard)) (null upgrade))
              (error "define-package: bad :upgrade directive"))
            (setf upgrade (car args)) :else
      :do (error "unrecognized define-package keyword ~S" kw)
      :finally (return `(:nicknames ,nicknames :documentation ,documentation
                         :use ,(if use-p use '(:common-lisp))
                         :shadow ,shadow :shadowing-import-from ,shadowing-import-from
                         :import-from ,import-from :export ,export :intern ,intern
                         :recycle ,(if recycle-p recycle (cons package nicknames))
                         :mix ,mix :reexport ,reexport :unintern ,unintern
                         ,@(when upgrade `(:upgrade ,upgrade))
                         :fmakunbound ,fmakunbound :fmakunbound-setf ,fmakunbound-setf)))))

(defmacro define-package (package &rest clauses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+gcl (defpackage ,package (:use))
     (apply 'ensure-package ',(parse-define-package-clauses package clauses))))
