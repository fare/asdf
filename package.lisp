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
  (defun symbol-package-name (symbol)
    (let ((package (symbol-package symbol)))
      (and package (package-name package))))
  (defun symbol-vector (symbol)
    (vector (symbol-name symbol) (symbol-package-name symbol)))
  (defun vector-symbol (vector)
    (let* ((symbol-name (aref vector 0))
           (package-name (aref vector 1)))
      (if package-name (intern symbol-name package-name)
          (make-symbol symbol-name))))
  (defun home-package-p (symbol package)
    (eq (symbol-package symbol) (find-package* package))))

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
  (defun delete-package* (package &optional nuke)
    (let ((p (find-package package)))
      (when p
        (when nuke (do-symbols (s p) (when (eq p (symbol-package s)) (nuke-symbol s))))
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
  (defun package-definition-form (package-designator &key internp (nicknamesp t) (error t))
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
             (:use ,@(sort-names use))
             ,@(when-relevant :shadow (sort-names shadow))
             ,@(import-options :shadowing-import-from shadowing-import)
             ,@(import-options :import-from import)
             ,@(when-relevant :export (sort-names export))
             ,@(when-relevant :intern (and internp (sort-names intern)))))))))


;;; ensure-package, define-package

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *fishy-package-changes* '(t))
  (defun ensure-package (name &key
                                (fishyp *fishy-package-changes*)
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
           ;; string to list canonical package and providing package:
           (inherited (make-hash-table :test 'equal))
           (fishy ())) ; fishy stuff we did
      (macrolet ((fishy (&rest info)
                   `(when fishyp (push (list ,@info) fishy))))
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
                      (when status
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
                          (fishy :recycled-duplicate name (package-name foundp) (package-name r))
                          (nuke-symbol symbol))
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
                          (fishy :ensure-symbol name (symbol-package-name existing) status intern))
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
                    (if n (rename-package p (first n) (rest n))
                        (progn
                          (rename-package-away p)
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
          (when fishy (push (cons name fishy) *fishy-package-changes*))
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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+(or ecl gcl) (defpackage ,package (:use))
     #+clisp (macrolet ((foo ()
                          (apply 'ensure-package ',(parse-define-package-form package clauses))
                          (package-definition-form ',package :nicknamesp nil)))
               (foo))
     (apply 'ensure-package ',(parse-define-package-form package clauses))))

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf2.27 *features*)
    (when (find-package :asdf)
      (delete-package* :asdf t))
    (make-package :asdf :use ())))
