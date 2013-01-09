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
   #:find-package* #:package-name* #:find-symbol* #:intern* #:unintern*
   #:unlink-package #:ensure-package #:define-package #:package-data))

(in-package :asdf/package)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defmacro DBG (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of variable and their values, returns the last value"
  ;"if not in debugging mode, just compute and return last value"
  #-DBGXXX (declare (ignore tag)) #-DBGXXX (car (last exprs)) #+DBGXXX
  (let ((res (gensym))(f (gensym)))
  `(let (,res (*print-readably* nil))
    (flet ((,f (fmt &rest args) (apply #'format *error-output* fmt args)))
      (fresh-line *standard-output*) (fresh-line *trace-output*) (fresh-line *error-output*)
      (,f "~&~A~%" ,tag)
      ,@(mapcan
         #'(lambda (x)
            `((,f "~&  ~S => " ',x)
              (,f "~{~S~^ ~}~%" (setf ,res (multiple-value-list ,x)))))
         exprs)
      (apply 'values ,res)))))

;;;; General purpose package utilities

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun find-package* (package-designator &optional (error t))
    (let ((package (find-package package-designator)))
      (cond
        (package package)
        (error (error "No package named ~S" (string package-designator)))
        (t nil))))
  (defun package-name* (package-designator &optional (error t))
    (let ((package (find-package* package-designator error)))
      (when package (package-name package))))
  (defun find-symbol* (name package-designator &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unless CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package
          (multiple-value-bind (symbol status) (find-symbol (string name) package)
            (cond
              (status (return (values symbol status)))
              (error (error "There is no symbol ~S in package ~S" name (package-name package))))))
        (values nil nil))))
  (defun intern* (name package-designator &optional (error t))
    (intern (string name) (package-name* package-designator error)))
  (defun unintern* (name package-designator &optional (error t))
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package
          (multiple-value-bind (symbol status) (find-symbol* name package error)
            (cond
              (status (unintern symbol package)
                      (return (values symbol status)))
              (error (error "symbol ~A not present in package ~A"
                            (string symbol) (package-name package))))))
        (values nil nil))))
  (defun package-data (package-designator &optional (error t))
    (let ((package (find-package* package-designator error)))
      (when package
        (labels ((string-sort (strings)
                   (sort strings #'string<))
                 (sort-packages (packages)
                   (string-sort (mapcar #'package-name packages))))
          (loop :with internal :with external :with inherited
                :for sym :being :the :symbols :in package
                :for status = (nth-value 1 (find-symbol* sym package)) :do
                  (ecase status
                    (:internal (push sym internal))
                    (:external (push sym external))
                    (:inherited (push sym inherited)))
                :finally
                   (return
                     `(:name ,(package-name package)
                       :nicknames ,(package-nicknames package)
                       :internal ,(string-sort internal)
                       :external ,(string-sort external)
                       :inherited ,(string-sort inherited)
                       :shadowing ,(string-sort (package-shadowing-symbols package))
                       :use ,(sort-packages (package-use-list package))
                       :used-by ,(sort-packages (package-used-by-list package))))))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun soft-upgrade-p (upgrade)
    (ecase upgrade ((:soft nil) t) (:hard nil)))
  (defun ensure-package-unused (package)
    (loop :for p :in (package-used-by-list package) :do
      (unuse-package package p)))
  (defun ensure-package-deleted (package) ;; &key upgrade
    (let ((p (find-package package)))
      (when p
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
  (defun recycle-symbol (name recycle)
    (loop :for r :in recycle
          :for s = (find-symbol* name r nil)
          :when s :do (return (values s r))))
  (defun ensure-package (name &key
                                upgrade
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern fmakunbound fmakunbound-setf)
    (let* ((nicknames (mapcar #'string nicknames))
           (shadow (mapcar #'string shadow))
           (shadowing-import-from (loop :for sif :in shadowing-import-from
                                        :collect (mapcar #'string sif)))
           (import-from (loop :for if :in import-from
                              :collect (mapcar #'string if)))
           (export (mapcar #'string export))
           (recycle (remove nil (mapcar #'find-package recycle)))
           (shadowed (make-hash-table :test 'equal)) ; string to bool
           (imported (make-hash-table :test 'equal)) ; string to bool
           (exported (make-hash-table :test 'equal)) ; string to bool
           (inherited (make-hash-table :test 'equal)) ; string to package name
           (previous (remove-duplicates
                      (mapcar #'find-package (cons name nicknames))
                      :from-end t))
           (discarded (cdr previous))
           (package (or (first previous) (make-package name :nicknames nicknames))))
      (setf (documentation package t) documentation)
      (DBG :ensure-package name upgrade
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern fmakunbound fmakunbound-setf)
      ;;#+DBG (untrace)(trace find-symbol make-package delete-package use-package unuse-package import export intern shadow shadowing-import unintern unexport)
      (assert (soft-upgrade-p upgrade))
      (ensure-package-unused package)
      (map () #'ensure-package-deleted discarded)
      (rename-package package name nicknames)
      (dolist (name unintern) (unintern* name package))
      ;;; Compute the desired state of the package
      (loop :for sym :in shadow :for name = (string sym) :do
        (DBG :sha name)
        (setf (gethash name shadowed) t)
        (multiple-value-bind (recycled previous) (recycle-symbol name recycle)
          (cond
            ((or (not previous) (not (member (symbol-package recycle) recycle)))
             (ecase (nth-value 1 (find-symbol* name package nil))
               ((nil :inherited) (shadow name package))
               ((:internal :external) (shadowing-import (make-symbol name) package))))
            ((eq previous package) (shadow recycled package))
            (t (unintern* recycled previous) (shadowing-import recycled package)))))
      (labels
          ((ensure-shadowing-import (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (cond
                 ((gethash name shadowed)
                  (unless (eq i (find-symbol* name package))
                    (error "Conflicting shadowings for ~A" name)))
                 (t
                    (setf (gethash name shadowed) t)
                    (setf (gethash name imported) t)
                    (shadowing-import package)))))
           (ensure-import (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (multiple-value-bind (x xp) (find-symbol name package)
                 (cond
                   ((gethash name imported)
                    (unless (eq i x)
                      (error "Can't import ~S from both ~S and ~S"
                             name (package-name (symbol-package x)) (package-name p))))
                   ((gethash name shadowed)
                    (error "Can't both shadow ~S and import it from ~S" name (package-name p)))
                   (t
                    (when (and xp (not (eq i x)))
                      (unintern* x package))
                    (setf (gethash name imported) t)
                    (import i package))))))
           (ensure-mix (sym p)
             (let* ((name (string sym))
                    (sp (string p)))
               (unless (or (gethash name shadowed) (gethash name imported))
                 (let ((ip (gethash name inherited)))
                   (cond
                     ((eq sp ip))
                     (ip
                      (remhash name inherited)
                      (ensure-shadowing-import name ip))
                     (t
                      (ensure-inherited sym sp)))))))
           (ensure-inherited (sym p)
             (let* ((name (string sym))
                    (sp (string p))
                    (s (find-symbol* name sp))
                    (ip (gethash name inherited)))
               (multiple-value-bind (x xp) (find-symbol name package)
                 (cond
                   (ip
                    (unless (eq ip sp)
                      (error "Can't inherit ~S from ~S, it is inherited from ~S"
                             name sp ip)))
                   ((gethash name imported)
                    (unless (eq s x)
                      (error "Can't inherit ~S from ~S, it is imported from ~S"
                             name sp (package-name (symbol-package x)))))
                   ((gethash name shadowed)
                    (error "Can't inherit ~S from ~S, it is shadowed" name sp))
                   (t
                    (when xp
                      (unintern* x package)))))))
           (ensure-registered (sym)
             (let ((name (string sym)))
               (unless (or (gethash name shadowed)
                           (gethash name imported)
                           (gethash name inherited))
                 (multiple-value-bind (recycled previous) (recycle-symbol name recycle)
                   (cond
                       ((or (not previous) (not (member (symbol-package recycled) recycle)))
                        (unintern* sym package))
                       ((eq previous package))
                       (t (unintern* recycled previous) (import recycled package))))))))
        (loop :for (p . syms) :in shadowing-import-from :do
          (DBG :shaif p syms)
          (dolist (sym syms) (ensure-shadowing-import sym p)))
        (loop :for p :in mix :do
          (DBG :mix p)
          (do-external-symbols (sym p) (ensure-mix sym p)))
        (loop :for (p . syms) :in import-from :do
          (DBG :if p syms)
          (dolist (sym syms) (ensure-import sym p)))
        (loop :for p :in use :for sp = (string p) :for pp = (find-package sp) :do
          (DBG :use p sp pp)
          (do-external-symbols (sym pp) (ensure-inherited sym sp))
          (use-package pp package))
        (DBG :intern intern)
        (dolist (sym intern) (intern* sym package))
        (do-symbols (sym package)
          (DBG :ers sym)
          (ensure-registered sym))
        (loop :for p :in reexport :do
          (DBG :reex p)
          (do-external-symbols (sym p)
            (let ((name (string sym)))
              (export (find-symbol* name package) package) (setf (gethash name exported) t))))
        (DBG :export export)
        (loop :for sym :in export :for name = (string sym) :for symbol = (intern* name package) :do
          (export symbol package) (setf (gethash name exported) t))
        (DBG :unexport)
        (do-external-symbols (sym package) (unless (gethash (symbol-name sym) exported) (unexport sym package)))
        ;; do away with packages with conflicting (nick)names
        ;; note from ASDF 2.26: ECL might not be liking an early fmakunbound (below #-ecl'ed)
        (ensure-package-fmakunbound package fmakunbound)
        (ensure-package-fmakunbound-setf package fmakunbound-setf)
        ;;#+DBG (untrace)
        package))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-define-package-form (package clauses)
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
      :finally (return `(,package
                         :nicknames ,nicknames :documentation ,documentation
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
     (apply 'ensure-package ',(parse-define-package-form package clauses))))
