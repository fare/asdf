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
   #:find-package* #:find-symbol* #:symbol-call #:intern* #:unintern*
   #:symbol-shadowing-p #:rehome-symbol
   #:delete-package* #:package-names #:packages-from-names
   #:package-definition-form #:ensure-package #:define-package))

(in-package :asdf/package)

(defmacro DBG (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of variable and their values, returns the last value"
  ;;"if not in debugging mode, just compute and return last value"
  ;; #-DBGXXX (declare (ignore tag)) #-DBGXXX (car (last exprs)) #+DBGXXX
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
  (defun symbol-call (package name &rest args)
    "Call a function associated with symbol of given name in given package,
with given ARGS. Useful when the call is read before the package is loaded,
or when loading the package is optional."
    (apply (find-symbol* name package) args))
  (defun intern* (name package-designator &optional (error t))
    (intern (string name) (find-package* package-designator error)))
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
  (defun symbol-shadowing-p (symbol package)
    (member symbol (package-shadowing-symbols package)))
  #+(or clisp clozure)
  (defun get-setf-function-symbol (symbol)
    #+clisp (let ((sym (get symbol 'system::setf-function)))
              (if sym (values sym :setf-function)
                  (let ((sym (get symbol 'system::setf-expander)))
                    (if sym (values sym :setf-expander)
                        (values nil nil)))))
    #+clozure (gethash symbol ccl::%setf-function-names%))
  #+(or clisp clozure)
  (defun set-setf-function-symbol (new-setf-symbol symbol &optional kind)
    #+clisp (assert (member kind '(:setf-function :setf-expander)))
    #+clozure (assert (eq kind t))
    #+clisp
    (cond
      ((null new-setf-symbol)
       (remprop symbol 'system::setf-function)
       (remprop symbol 'system::setf-expander))
      ((eq kind :setf-function)
       (setf (get symbol 'system::setf-function) new-setf-symbol))
      ((eq kind :setf-expander)
       (setf (get symbol 'system::setf-expander) new-setf-symbol)))
    #+clozure
    (progn
      (gethash symbol ccl::%setf-function-names%) new-setf-symbol
      (gethash new-setf-symbol ccl::%setf-function-name-inverses%) symbol))
  #+(or clisp clozure)
  (defun create-setf-function-symbol (symbol)
    #+clisp (system::setf-symbol symbol)
    #+clozure (ccl::construct-setf-function-name symbol))
  (defun rehome-symbol (symbol package-designator)
    "Changes the home package of a symbol, also leaving it present in its old home if any"
    (let* ((name (symbol-name symbol))
           (package (find-package* package-designator))
           (old-package (symbol-package symbol))
           (old-status (and old-package (nth-value 1 (find-symbol name old-package))))
           (shadowing (and old-package (symbol-shadowing-p symbol old-package) (make-symbol name))))
      (multiple-value-bind (overwritten-symbol overwritten-symbol-status) (find-symbol name package)
        (unless (eq package old-package)
          (let ((overwritten-symbol-shadowing-p
                  (and overwritten-symbol-status
                       (symbol-shadowing-p overwritten-symbol package))))
            (when old-package
              (if shadowing
                  (shadowing-import shadowing old-package))
                  (unintern symbol old-package))
            (cond
              (overwritten-symbol-shadowing-p
               (shadowing-import symbol package))
              (t
               (when overwritten-symbol-status
                 (unintern overwritten-symbol package))
               (import symbol package)))
            (if shadowing
                (shadowing-import symbol old-package)
                (import symbol old-package))
            #+(or clisp clozure)
            (multiple-value-bind (setf-symbol kind)
                (get-setf-function-symbol symbol)
              (when kind
                (let* ((setf-function (fdefinition setf-symbol))
                       (new-setf-symbol (create-setf-function-symbol symbol)))
                  (setf (fdefinition new-setf-symbol) setf-function)
                  (set-setf-function-symbol symbol new-setf-symbol kind))))
            #+(or clisp clozure)
            (multiple-value-bind (overwritten-setf foundp)
                (get-setf-function-symbol overwritten-symbol)
              (when foundp
                (unintern overwritten-setf)))
            (when (eq old-status :external)
              (export symbol old-package))
            (when (eq overwritten-symbol-status :external)
              (export symbol package))))
        (values overwritten-symbol overwritten-symbol-status))))
  (defun ensure-package-unused (package)
    (loop :for p :in (package-used-by-list package) :do
      (unuse-package package p)))
  (defun delete-package* (package)
    (let ((p (find-package package)))
      (when p
        (ensure-package-unused p)
        (delete-package package))))
  (defun package-names (package)
    (cons (package-name package) (package-nicknames package)))
  (defun packages-from-names (names)
    (remove-duplicates (remove nil (mapcar #'find-package names)) :from-end t)))


;;; Communicable representation of symbol and package information

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun package-definition-form (package-designator &key internp (error t))
    (let* ((package (find-package* package-designator error))
           (name (package-name package))
           (nicknames (package-nicknames package))
           (use (mapcar #'package-name (package-use-list package)))
           (shadow ())
           (shadowing-import (make-hash-table :test 'equal))
           (import (make-hash-table :test 'equal))
           (export ())
           (intern ()))
      (when package
        (loop :for sym :being :the :symbols :in package
              :for status = (nth-value 1 (find-symbol* sym package)) :do
                (ecase status
                  ((nil :inherited))
                  ((:internal :external)
                   (let* ((name (symbol-name sym))
                          (external (eq status :external))
                          (home (symbol-package sym))
                          (home-name (package-name home))
                          (imported (not (eq home package)))
                          (shadowing (symbol-shadowing-p sym package)))
                     (cond
                       ((and shadowing import)
                        (push name (gethash home-name shadowing-import)))
                       (shadowing
                        (push name shadow))
                       (imported
                        (push name (gethash home-name import))))
                     (cond
                       (external
                        (push name export))
                       (imported)
                       (t (push name intern)))))))
        (labels ((sort-names (names)
                   (sort names #'string<))
                 (table-keys (table)
                   (loop :for k :being :the :hash-keys :of table :collect k))
                 (when-relevant (key value)
                   (when value (list (cons key value))))
                 (import-options (key table)
                   (loop :for i :in (sort-names (table-keys table))
                         :collect `(,key ,i ,@(sort-names (gethash i table))))))
          `(defpackage ,name
             ,@(when-relevant :nicknames (sort-names nicknames))
             (:use ,@(sort-names use))
             ,@(when-relevant :shadow (sort-names shadow))
             ,@(import-options :shadowing-import-from shadowing-import)
             ,@(import-options :import-from import)
             ,@(when-relevant :export (sort-names export))
             ,@(when-relevant :intern (and internp (sort-names intern)))))))))


;;; ensure-package, define-package

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun ensure-package (name &key
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern)
    (let* ((name (string name))
           (nicknames (mapcar #'string nicknames))
           (names (cons name nicknames))
           (previous (packages-from-names names))
           (discarded (cdr previous))
           (to-delete ())
           (package (or (first previous) (make-package name :nicknames nicknames)))
           (recycle (packages-from-names recycle))
           (shadowed (make-hash-table :test 'equal)) ; string to bool
           (imported (make-hash-table :test 'equal)) ; string to bool
           (exported (make-hash-table :test 'equal)) ; string to bool
           (inherited (make-hash-table :test 'equal))) ; string to package name
      (labels
          ((fresh-package-name (&optional (prefix :%TO-BE-DELETED)
                                  (index (random most-positive-fixnum)))
             (loop :for i :from index
                   :for n = (format nil "~A-~D" prefix i)
                   :thereis (and (not (find-package n)) n)))
           (rename-package-away (p)
             (rename-package p (fresh-package-name)))
           (ensure-shadowing-import (sym p)
             (let* ((name (string sym))
                    (i (find-symbol* name p)))
               (cond
                 ((gethash name shadowed)
                  (unless (eq i (find-symbol* name package))
                    (error "Conflicting shadowings for ~A" name)))
                 (t
                    (setf (gethash name shadowed) t)
                    (setf (gethash name imported) t)
                    (shadowing-import i package)))))
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
                    (setf (gethash name imported) t)
                    (unless (and xp (eq i x))
                      (when xp (unintern* x p))
                      (import i package)))))))
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
                    (symbol (find-symbol* name p))
                    (sp (symbol-package symbol))
                    (spn (package-name sp))
                    (ipn (gethash name inherited)))
               (multiple-value-bind (x xp) (find-symbol name package)
                 (cond
                   ((gethash name shadowed))
                   (ipn
                    (unless (eq spn ipn)
                      (error "Can't inherit ~S from ~S, it is inherited from ~S"
                             name spn ipn)))
                   ((gethash name imported)
                    (unless (eq symbol x)
                      (error "Can't inherit ~S from ~S, it is imported from ~S"
                             name sp (package-name (symbol-package x)))))
                   (t
                    (setf (gethash name inherited) spn)
                    (when xp
                      (unintern* x package)))))))
           (recycle-symbol (name)
             (dolist (r recycle (values nil nil))
               (multiple-value-bind (symbol status) (find-symbol name r)
                 (when (and status (eq r (symbol-package symbol)))
                   (return (values symbol r))))))
           (symbol-recycled-p (sym)
             (member (symbol-package sym) recycle))
           (ensure-symbol (name &optional intern)
             (unless (or (gethash name shadowed)
                         (gethash name imported)
                         (gethash name inherited))
               (multiple-value-bind (recycled previous) (recycle-symbol name)
                 (cond
                   ((eq previous package))
                   ((or (not previous) (not (member (symbol-package recycled) recycle)))
                    (when intern (intern* name package)))
                   (t (rehome-symbol recycled package))))))
           (ensure-export (name p)
             (multiple-value-bind (symbol status) (find-symbol* name p)
               (assert status)
               (unless (eq status :external)
                 (ensure-exported name symbol p))))
           (ensure-exported (name sym p)
             (dolist (u (package-used-by-list p))
               (ensure-exported-to-user name sym u))
             (export sym p))
           (ensure-exported-to-user (name sym u)
             (multiple-value-bind (usym ustat) (find-symbol name u)
               (unless (eq sym usym)
                 (let ((shadowing (member usym (package-shadowing-symbols u))))
                   (block nil
                     (cond
                       ((not shadowing)
                        (unintern usym u))
                       ((symbol-recycled-p usym)
                        (shadowing-import sym u))
                       (t (return)))
                     (when (eq ustat :external)
                       (ensure-exported name sym u))))))))
        #-gcl (setf (documentation package t) documentation) #+gcl documentation
        (loop :for p :in discarded
              :for n = (remove-if #'(lambda (x) (member x names :test 'equal))
                                  (package-names p))
              :do (if n (rename-package p (first n) (rest n))
                      (progn
                        (rename-package-away p)
                        (push p to-delete))))
        (rename-package package name nicknames)
        (loop :for p :in (set-difference (package-use-list package) (append mix use))
              :do (unuse-package p package))
        (dolist (name unintern) (unintern* name package nil))
        (dolist (sym export) (setf (gethash (string sym) exported) t))
        (loop :for p :in reexport :do
          (do-external-symbols (sym p)
            (setf (gethash (string sym) exported) t)))
        (do-external-symbols (sym package)
          (unless (gethash (symbol-name sym) exported) (unexport sym package)))
        (loop :for s :in shadow :for name = (string s) :do
          (setf (gethash name shadowed) t)
          (multiple-value-bind (recycled previous) (recycle-symbol name)
            (cond
              ((or (not previous) (not (member (symbol-package recycle) recycle)))
               (ecase (nth-value 1 (find-symbol* name package nil))
                 ((nil :inherited) (shadow name package))
                 ((:internal :external) (shadowing-import (make-symbol name) package))))
              ((eq previous package) (shadow recycled package))
              (t (rehome-symbol recycled package)))))
        (loop :for (p . syms) :in shadowing-import-from :do
          (dolist (sym syms) (ensure-shadowing-import sym p)))
        (loop :for p :in mix :do
          (do-external-symbols (sym p) (ensure-mix sym p)))
        (loop :for (p . syms) :in import-from :do
          (dolist (sym syms) (ensure-import sym p)))
        (loop :for p :in (append use mix) :for pp = (find-package* p) :do
          (do-external-symbols (sym pp) (ensure-inherited sym pp))
          (use-package pp package))
        (loop :for name :being :the :hash-keys :of exported :do
          (ensure-symbol (string name) t)
          (ensure-export name package))
        (dolist (name intern)
          (ensure-symbol (string name) t))
        (do-symbols (sym package)
          (ensure-symbol (symbol-name sym)))
        (map () 'delete-package* to-delete)
        package))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-define-package-form (package clauses)
    (loop
      :with use-p = nil :with recycle-p = nil
      :with documentation = nil
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
      :do (error "unrecognized define-package keyword ~S" kw)
      :finally (return `(,package
                         :nicknames ,nicknames :documentation ,documentation
                         :use ,(if use-p use '(:common-lisp))
                         :shadow ,shadow :shadowing-import-from ,shadowing-import-from
                         :import-from ,import-from :export ,export :intern ,intern
                         :recycle ,(if recycle-p recycle (cons package nicknames))
                         :mix ,mix :reexport ,reexport :unintern ,unintern)))))

(defmacro define-package (package &rest clauses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+(or ecl gcl) (defpackage ,package (:use))
     #+clisp (macrolet ((foo ()
                          (apply 'ensure-package ',(parse-define-package-form package clauses))
                          (package-definition-form ',package)))
               (foo))
     (apply 'ensure-package ',(parse-define-package-form package clauses))))

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (find-package :asdf) (not (member :asdf2.27 *features*)))
    (delete-package* :asdf)))

