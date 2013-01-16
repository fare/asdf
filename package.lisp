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
   #:find-package* #:find-symbol* #:symbol-call #:intern* #:unintern* #:make-symbol*
   #:symbol-shadowing-p #:home-package-p #:rehome-symbol
   #:symbol-package-name #:standard-common-lisp-symbol-p
   #:reify-package #:unreify-package #:reify-symbol #:unreify-symbol
   #:nuke-symbol-in-package #:nuke-symbol
   #:ensure-package-unused #:delete-package*
   #:fresh-package-name #:rename-package-away #:package-names #:packages-from-names
   #:package-definition-form #:parse-define-package-form
   #:ensure-package #:define-package))

(in-package :asdf/package)

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
        (when package ;; package error handled by find-package* already
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
  (defun make-symbol* (name)
    (etypecase name
      (string (make-symbol name))
      (symbol (copy-symbol name))))
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
    (and (member symbol (package-shadowing-symbols package)) t))
  (defun home-package-p (symbol package)
    (and package (let ((sp (symbol-package symbol)))
                   (and sp (let ((pp (find-package* package)))
                             (and pp (eq sp pp))))))))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun symbol-package-name (symbol)
    (let ((package (symbol-package symbol)))
      (and package (package-name package))))
  (defun standard-common-lisp-symbol-p (symbol)
    (multiple-value-bind (sym status) (find-symbol* symbol :common-lisp nil)
      (and (eq sym symbol) (eq status :external))))
  (defun reify-package (package &optional package-context)
    (if (eq package package-context) t
        (etypecase package
          (null nil)
          ((eql (find-package :cl)) :cl)
          (package (package-name package)))))
  (defun unreify-package (package &optional package-context)
    (etypecase package
      (null nil)
      ((eql t) package-context)
      ((or symbol string) (find-package package))))
  (defun reify-symbol (symbol &optional package-context)
    (etypecase symbol
      ((or keyword (satisfies standard-common-lisp-symbol-p)) symbol)
      (symbol (vector (symbol-name symbol)
                      (reify-package (symbol-package symbol) package-context)))))
  (defun unreify-symbol (symbol &optional package-context)
    (etypecase symbol
      (symbol symbol)
      ((simple-vector 2)
       (let* ((symbol-name (svref symbol 0))
              (package-foo (svref symbol 1))
              (package (unreify-package package-foo package-context)))
         (if package (intern* symbol-name package)
             (make-symbol* symbol-name)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
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
  (defun set-dummy-symbol (symbol reason other-symbol)
    (setf (get symbol 'dummy-symbol) (cons reason other-symbol)))
  (defun make-dummy-symbol (symbol)
    (let ((dummy (copy-symbol symbol)))
      (set-dummy-symbol dummy 'replacing symbol)
      (set-dummy-symbol symbol 'replaced-by dummy)
      dummy))
  (defun dummy-symbol (symbol)
    (get symbol 'dummy-symbol))
  (defun get-dummy-symbol (symbol)
    (let ((existing (dummy-symbol symbol)))
      (if existing (values (cdr existing) (car existing))
          (make-dummy-symbol symbol))))
  (defun nuke-symbol-in-package (symbol package-designator)
    (let ((package (find-package* package-designator))
          (name (symbol-name symbol)))
      (multiple-value-bind (sym stat) (find-symbol name package)
        (when (and (member stat '(:internal :external)) (eq symbol sym))
          (if (symbol-shadowing-p symbol package)
              (shadowing-import (get-dummy-symbol symbol) package)
              (unintern symbol package))))))
  (defun nuke-symbol (symbol &optional (packages (list-all-packages)))
    #+(or clisp clozure)
    (multiple-value-bind (setf-symbol kind)
        (get-setf-function-symbol symbol)
      (when kind (nuke-symbol setf-symbol)))
    (loop :for p :in packages :do (nuke-symbol-in-package symbol p)))
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
  (defun delete-package* (package &key nuke)
    (let ((p (find-package package)))
      (when p
        (when nuke (do-symbols (s p) (when (home-package-p s p) (nuke-symbol s))))
        (ensure-package-unused p)
        (delete-package package))))
  (defun fresh-package-name (&optional (prefix :%TO-BE-DELETED)
                               (index (random most-positive-fixnum)))
    (loop :for i :from index
          :for n = (format nil "~A-~D" prefix i)
          :thereis (and (not (find-package n)) n)))
  (defun rename-package-away (p)
    (rename-package
     p (fresh-package-name (format nil "__~A__" (package-name p)) 0)))
  (defun package-names (package)
    (cons (package-name package) (package-nicknames package)))
  (defun packages-from-names (names)
    (remove-duplicates (remove nil (mapcar #'find-package names)) :from-end t)))


;;; Communicable representation of symbol and package information

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun package-definition-form (package-designator
                                  &key (nicknamesp t) (usep t)
                                    (shadowp t) (shadowing-import-p t)
                                    (exportp t) (importp t) internp (error t))
    (let* ((package (or (find-package* package-designator error)
                        (return-from package-definition-form nil)))
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
                       ((and shadowing imported)
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
             ,@(when-relevant :nicknames (and nicknamesp (sort-names nicknames)))
             (:use ,@(and usep (sort-names use)))
             ,@(when-relevant :shadow (and shadowp (sort-names shadow)))
             ,@(import-options :shadowing-import-from (and shadowing-import-p shadowing-import))
             ,@(import-options :import-from (and importp import))
             ,@(when-relevant :export (and exportp (sort-names export)))
             ,@(when-relevant :intern (and internp (sort-names intern)))))))))


;;; ensure-package, define-package

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *all-package-happiness* '())
  (defvar *all-package-fishiness* (list t))
  (defvar *package-fishiness* '())
  (defun flush-fishy ()
    (when *package-fishiness*
      (if (null (rest *package-fishiness*))
          (push (first *package-fishiness*) *all-package-happiness*)
          (push (nreverse *package-fishiness*) *all-package-fishiness*))
      (setf *package-fishiness* nil)))
  (defun record-fishy (info)
    (push info *package-fishiness*))
  (defun ensure-package (name &key
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern)
    (macrolet ((when-fishy (&body body)
                 `(when *all-package-fishiness* ,@body))
               (fishy (&rest info)
                 `(when-fishy (record-fishy (list ,@info)))))
      (let* ((name (string name))
             (nicknames (mapcar #'string nicknames))
             (names (cons name nicknames))
             (previous (packages-from-names names))
             (discarded (cdr previous))
             (to-delete ())
             (package (or (first previous) (make-package name :nicknames nicknames)))
             (recycle (packages-from-names recycle))
             (use (mapcar 'find-package* use))
             (mix (mapcar 'find-package* mix))
             (reexport (mapcar 'find-package* reexport))
             (shadow (mapcar 'string shadow))
             (export (mapcar 'string export))
             (intern (mapcar 'string intern))
             (unintern (mapcar 'string unintern))
             (shadowed (make-hash-table :test 'equal)) ; string to bool
             (imported (make-hash-table :test 'equal)) ; string to bool
             (exported (make-hash-table :test 'equal)) ; string to bool
             ;; string to list home package and use package:
             (inherited (make-hash-table :test 'equal)))
        (when-fishy (record-fishy name))
        (labels
            ((ensure-shadowing-import (name p)
               (let ((import (find-symbol* name p)))
                 (multiple-value-bind (existing status) (find-symbol name package)
                   (cond
                     ((gethash name shadowed)
                      (unless (eq import existing)
                        (error "Conflicting shadowings for ~A" name)))
                     (t
                      (setf (gethash name shadowed) t)
                      (setf (gethash name imported) t)
                      (unless (or (null status)
                                  (and (member status '(:internal :external))
                                       (eq existing import)
                                       (symbol-shadowing-p existing package)))
                        (fishy :shadowing-import
                               name (package-name p) (symbol-package-name import)
                               (and status (symbol-package-name existing)) status))
                      (shadowing-import import package))))))
             (ensure-import (sym p)
               (let* ((name (string sym))
                      (import (find-symbol* name p)))
                 (multiple-value-bind (existing status) (find-symbol name package)
                   (cond
                     ((gethash name imported)
                      (unless (eq import existing)
                        (error "Can't import ~S from both ~S and ~S"
                               name (package-name (symbol-package existing)) (package-name p))))
                     ((gethash name shadowed)
                      (error "Can't both shadow ~S and import it from ~S" name (package-name p)))
                     (t
                      (setf (gethash name imported) t)
                      (unless (and status (eq import existing))
                        (when status
                          (unintern* existing package)
                          (fishy :import name (package-name p) (symbol-package-name import)
                                 (and status (symbol-package-name existing)) status))
                        (import import package)))))))
             (ensure-mix (name symbol p)
               (unless (gethash name shadowed)
                 (multiple-value-bind (existing status) (find-symbol name package)
                   (let* ((sp (symbol-package symbol))
                          (im (gethash name imported))
                          (in (gethash name inherited)))
                     (cond
                       ((or (null status)
                            (and status (eq symbol existing))
                            (and in (eq sp (first in))))
                        (ensure-inherited name symbol p t))
                       (in
                        (remhash name inherited)
                        (ensure-shadowing-import name (second in)))
                       (im
                         (error "Imported symbol ~S conflicts with inherited symbol ~S in ~S"
                                existing symbol (package-name package)))
                       (t
                        (ensure-inherited name symbol p t)))))))
             (ensure-inherited (name symbol p mix)
               (multiple-value-bind (existing status) (find-symbol name package)
                 (let* ((sp (symbol-package symbol))
                        (in (gethash name inherited))
                        (xp (and status (symbol-package existing))))
                   (cond
                     ((gethash name shadowed))
                     (in
                      (unless (equal sp (first in))
                        (if mix
                            (ensure-shadowing-import name (second in))
                            (error "Can't inherit ~S from ~S, it is inherited from ~S"
                                   name (package-name sp) (package-name (first in))))))
                     ((gethash name imported)
                      (unless (eq symbol existing)
                        (error "Can't inherit ~S from ~S, it is imported from ~S"
                               name (package-name sp) (package-name xp))))
                     (t
                      (setf (gethash name inherited) (list sp p))
                      (when (and status (not (eq sp xp)))
                        (let ((shadowing (symbol-shadowing-p existing package)))
                          (fishy :inherited name (package-name p) (package-name sp)
                                 (package-name xp))
                          (if shadowing (ensure-shadowing-import name p)
                              (unintern* existing package)))))))))
             (recycle-symbol (name)
               (let (recycled foundp)
                 (dolist (r recycle (values recycled foundp))
                   (multiple-value-bind (symbol status) (find-symbol name r)
                     (when (and status (home-package-p symbol r))
                       (cond
                         (foundp
                          ;; (nuke-symbol symbol)) -- even simple variable names like O or C will do that.
                          (fishy :recycled-duplicate name (package-name foundp) (package-name r)))
                         (t
                          (setf recycled symbol foundp r))))))))
             (symbol-recycled-p (sym)
               (member (symbol-package sym) recycle))
             (ensure-symbol (name &optional intern)
               (unless (or (gethash name shadowed)
                           (gethash name imported)
                           (gethash name inherited))
                 (multiple-value-bind (existing status)
                     (find-symbol name package)
                   (multiple-value-bind (recycled previous) (recycle-symbol name)
                     (cond
                       ((and status (eq existing recycled) (eq previous package)))
                       (previous
                        (rehome-symbol recycled package))
                       ((and status (eq package (symbol-package existing))))
                       (t
                        (when status
                          (unintern existing)
                          (fishy :ensure-symbol name
                                 (reify-package (symbol-package existing) package)
                                 status intern))
                        (when intern
                          (intern* name package))))))))
             (ensure-export (name p)
               (multiple-value-bind (symbol status) (find-symbol* name p)
                 (unless (eq status :external)
                   (ensure-exported name symbol p))))
             (ensure-exported (name sym p)
               (dolist (u (package-used-by-list p))
                 (ensure-exported-to-user name sym u))
               (export sym p))
             (ensure-exported-to-user (name sym u)
               (multiple-value-bind (usym ustat) (find-symbol name u)
                 (unless (and ustat (eq sym usym))
                   (let ((accessible
                           (when ustat
                             (let ((shadowing (symbol-shadowing-p usym u))
                                   (recycled (symbol-recycled-p usym)))
                               (unless (and shadowing (not recycled))
                                 (if (or (eq ustat :inherited) shadowing)
                                     (shadowing-import sym u)
                                     (unintern usym u))
                                 (fishy :ensure-export name (symbol-package-name sym)
                                        (package-name u)
                                        (and ustat (symbol-package-name usym)) ustat shadowing)
                                 t)))))
                     (when (and accessible (eq ustat :external))
                       (ensure-exported name sym u)))))))
          #-gcl (setf (documentation package t) documentation) #+gcl documentation
          (loop :for p :in (set-difference (package-use-list package) (append mix use))
                :do (unuse-package p package) (fishy :use (package-names p)))
          (loop :for p :in discarded
                :for n = (remove-if #'(lambda (x) (member x names :test 'equal))
                                    (package-names p))
                :do (fishy :nickname (package-names p))
                    (cond (n (rename-package p (first n) (rest n)))
                          (t (rename-package-away p)
                             (push p to-delete))))
          (rename-package package name nicknames)
          (dolist (name unintern)
            (multiple-value-bind (existing status) (find-symbol name package)
              (when status
                (unless (eq status :inherited)
                  (unintern* name package nil))
                (fishy :unintern name (symbol-package-name existing) status))))
          (dolist (name export)
            (setf (gethash name exported) t))
          (dolist (p reexport)
            (do-external-symbols (sym p)
              (setf (gethash (string sym) exported) t)))
          (do-external-symbols (sym package)
            (let ((name (symbol-name sym)))
              (unless (gethash name exported)
                (fishy :over-exported name (symbol-package-name sym))
                (unexport sym package))))
          (dolist (name shadow)
            (setf (gethash name shadowed) t)
            (multiple-value-bind (existing status) (find-symbol name package)
              (multiple-value-bind (recycled previous) (recycle-symbol name)
                (let ((shadowing (and status (symbol-shadowing-p existing package))))
                  (cond
                    ((eq previous package))
                    (previous
                     (fishy :shadow-recycled name (package-name previous)
                            (and status (symbol-package-name existing)) status shadowing)
                     (rehome-symbol recycled package))
                    ((or (member status '(nil :inherited))
                         (home-package-p existing package)))
                    (t
                     (let ((dummy (make-symbol name)))
                       (fishy :shadow-imported name (symbol-package-name existing) status shadowing)
                       (shadowing-import dummy package)
                       (import dummy package)))))))
            (shadow name package))
          (loop :for (p . syms) :in shadowing-import-from
                :for pp = (find-package* p) :do
                  (dolist (sym syms) (ensure-shadowing-import (string sym) pp)))
          (dolist (p mix)
            (do-external-symbols (sym p) (ensure-mix (symbol-name sym) sym p)))
          (loop :for (p . syms) :in import-from :do
            (dolist (sym syms) (ensure-import sym p)))
          (dolist (p (append use mix))
            (do-external-symbols (sym p) (ensure-inherited (string sym) sym p nil))
            (use-package p package))
          (loop :for name :being :the :hash-keys :of exported :do
            (ensure-symbol (string name) t)
            (ensure-export name package))
          (dolist (name intern)
            (ensure-symbol (string name) t))
          (do-symbols (sym package)
            (ensure-symbol (symbol-name sym)))
          (map () 'delete-package* to-delete)
          (flush-fishy)
          package)))))

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
  (let ((ensure-form
          `(apply 'ensure-package ',(parse-define-package-form package clauses))))
    (eval ensure-form)
    `(progn
       #+clisp
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,ensure-form)
       #+(or ecl gcl) (defpackage ,package (:use))
       #+clisp ,(package-definition-form package :error nil)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,ensure-form))))

;;;; Final tricks to keep various implementations happy.
(eval-when (:load-toplevel :compile-toplevel :execute)
  #+allegro ;; We need to disable autoloading BEFORE any mention of package ASDF.
  (setf excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car))) 

