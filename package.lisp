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
   #:find-symbol* #:define-package))

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
    (let ((sym (find-symbol* symbol package)))
      (when sym
        #-cormanlisp (unexport sym package)
        (unintern sym package)
        sym)))
  (defun present-symbol-p (symbol package)
    (member (nth-value 1 (find-symbol* symbol package)) '(:internal :external)))
  (defun present-symbols (package)
    ;; #-genera (loop :for s :being :the :present-symbols :in package :collect s) #+genera
    (let (l)
      (do-symbols (s package)
        (when (present-symbol-p s package) (push s l)))
      (reverse l)))
  (defun ensure-package-use (package use)
    (dolist (used (package-use-list package))
      (unless (member (package-name used) use :test 'string=)
        (unuse-package used)
        (do-external-symbols (sym used)
          (when (eq sym (find-symbol* sym package))
            (remove-symbol sym package)))))
    (dolist (used (reverse use))
      (do-external-symbols (sym used)
        (unless (eq sym (find-symbol* sym package))
          (remove-symbol sym package)))
      (use-package used package)))
  (defun ensure-package-unintern (package symbols)
    (loop :with packages = (list-all-packages)
          :for sym :in symbols
          :for removed = (remove-symbol sym package)
          :when removed :do
            (loop :for p :in packages :do
              (when (eq removed (find-symbol* sym p))
                (unintern removed p)))))
  (defun unlink-package (package)
    (let ((u (find-package package)))
      (when u
        (ensure-package-unintern u (present-symbols u))
        (loop :for p :in (package-used-by-list u) :do
          (unuse-package u p))
        (delete-package u))))
  (defun ensure-package-exists (name nicknames use)
    (let ((previous
            (remove-duplicates
             (mapcar #'find-package (cons name nicknames))
             :from-end t)))
      ;; do away with packages with conflicting (nick)names
      (map () #'unlink-package (cdr previous))
      ;; reuse previous package with same name
      (let ((p (car previous)))
        (cond
          (p
           (rename-package p name nicknames)
           (ensure-package-use p use)
           p)
          (t
           (make-package name :nicknames nicknames :use use))))))
  (defun ensure-package-shadow (package symbols)
    (shadow symbols package))
  (defun ensure-package-fmakunbound (package symbols)
    (loop :for name :in symbols
          :for sym = (find-symbol* name package)
          :when sym :do (fmakunbound sym)))
  (defun ensure-package-fmakunbound-setf (package symbols)
    (loop :for name :in symbols
          :for sym = (find-symbol* name package)
          :when sym :do #-gcl (fmakunbound `(setf ,sym))))
  (defun ensure-package-export (package export)
    (let ((formerly-exported-symbols nil)
          (bothly-exported-symbols nil)
          (newly-exported-symbols nil))
      (do-external-symbols (sym package)
        (if (member sym export :test 'string-equal)
            (push sym bothly-exported-symbols)
            (push sym formerly-exported-symbols)))
      (loop :for sym :in export :do
        (unless (member sym bothly-exported-symbols :test 'equal)
          (push sym newly-exported-symbols)))
      (loop :for user :in (package-used-by-list package)
            :for shadowing = (package-shadowing-symbols user) :do
              (loop :for new :in newly-exported-symbols
                    :for old = (find-symbol* new user)
                    :when (and old (not (member old shadowing)))
                      :do (unintern old user)))
      (loop :for x :in newly-exported-symbols :do
        (export (intern* x package)))))
  (defun ensure-package (name &key
                                nicknames use intern unintern shadow export
                                import-from shadowing-import-from
                                recycle mix fmakunbound fmakunbound-setf)
    recycle mix intern import-from shadowing-import-from
    (let* ((p (ensure-package-exists name nicknames use)))
      #-ecl (ensure-package-fmakunbound p fmakunbound) #+ecl fmakunbound ;; do it later on ECL
      #-ecl (ensure-package-fmakunbound-setf p fmakunbound-setf) #+ecl fmakunbound-setf
      (ensure-package-unintern p unintern)
      (ensure-package-shadow p shadow)
               (ensure-package-export p export)
               p))
#|
  (let ((h (make-hash-table :test 'equal)))
    (labels ((ensure-imported (n)
               (let* ((s (string n))
                      (x (gethash s h)))
                 (unless x (setf (gethash s h) t))
                 x))
             (import-from (package)
               (loop :for s :being :each :external-symbol :in package
                 :for n = (symbol-name s)
                 :unless (ensure-imported n)
                 :collect n)))
      ;; First, mark the symbols explicitly imported by the user
      (loop :for (kw . ()) :in clauses
            :when (member kw '(:import-from :shadowing-import-from)) :do
              (map () #'ensure-imported (cddr clauses)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (ensure-package
          ',name :nicknames ',nicknames :use ',use :export ',export
                 :shadow ',shadow :unintern ',unintern
                 :fmakunbound ',fmakunbound :fmakunbound-setf ',fmakunbound-setf)))
      `(defpackage ,package (:use)
         ,@(loop :for p :in mixed-packages
             :collect `(:import-from ,p ,@(import-from p)))
         ,@clauses
         (:export ,@(loop :for s :being :the :hash-keys :of h :collect s)))))))
|#
  (defun parse-define-package-clauses (clauses)
    (loop :for (kw . args) :in clauses
      :when (eq kw :nicknames) :append args :into nicknames :else
      :when (eq kw :use) :append args :into use :else
      :when (eq kw :shadow) :append args :into shadow :else
      :when (eq kw :export) :append args :into export :else
      :when (eq kw :intern) :append args :into intern :else
      :when (eq kw :import-from) :collect args :into import-from :else
      :when (eq kw :shadowing-import-from) :collect args :into shadowing-import-from :else
      :when (eq kw :recycle) :append args :into recycle :else
      :when (eq kw :mix) :append args :into mix :else
      :when (eq kw :unintern) :append args :into unintern :else
      :when (eq kw :fmakunbound) :append args :into fmakunbound :else
      :when (eq kw :fmakunbound-setf) :append args :into fmakunbound-setf :else
        :do (error "unrecognized define-package keyword ~S" kw)
      :finally (return `(:nicknames ,nicknames :use ,use
                         :shadow ,shadow :export ,export :intern ,intern
                         :import-from ,import-from :shadowing-import-from ,shadowing-import-from
                         :recycle ,recycle :mix ,mix :unintern ,unintern
                         :fmakunbound ,fmakunbound :fmakunbound-setf ,fmakunbound-setf))))
);eval-when

(defmacro define-package (package &rest clauses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (apply 'ensure-package ',package ',(parse-define-package-clauses clauses))))
