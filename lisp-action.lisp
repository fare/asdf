;;;; -------------------------------------------------------------------------
;;;; Actions to build Common Lisp software

(asdf/package:define-package :asdf/lisp-action
  (:recycle :asdf/lisp-action :asdf)
  (:intern #:proclamations #:flags)
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-component :asdf/operation :asdf/action)
  (:export
   #:try-recompiling
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:basic-load-op #:basic-compile-op #:compile-op-flags #:compile-op-proclamations
   #:load-op #:prepare-op #:compile-op #:test-op #:load-source-op #:prepare-source-op
   #:call-with-around-compile-hook
   #:perform-lisp-compilation #:perform-lisp-load-fasl #:perform-lisp-load-source))
(in-package :asdf/lisp-action)


;;;; Component classes
(defclass cl-source-file (source-file)
  ((type :initform "lisp")))
(defclass cl-source-file.cl (cl-source-file)
  ((type :initform "cl")))
(defclass cl-source-file.lsp (cl-source-file)
  ((type :initform "lsp")))


;;;; Operation classes
(defclass basic-load-op (operation) ())
(defclass basic-compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (flags :initarg :flags :accessor compile-op-flags
          :initform nil)))

;;; Our default operations: loading into the current lisp image
(defclass load-op (basic-load-op downward-operation sibling-operation) ())
(defclass prepare-op (upward-operation sibling-operation)
  ((sibling-operation :initform 'load-op :allocation :class)))
(defclass compile-op (basic-compile-op downward-operation)
  ((downward-operation :initform 'load-op :allocation :class)))

(defclass load-source-op (basic-load-op downward-operation) ())
(defclass prepare-source-op (upward-operation sibling-operation)
  ((sibling-operation :initform 'load-source-op :allocation :class)))

(defclass test-op (operation) ())


;;;; prepare-op, compile-op and load-op

;;; prepare-op
(defmethod operation-description ((o prepare-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading dependencies of ~3i~_~A~@:>") c))
(defmethod perform ((o prepare-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-op) (s system))
  (declare (ignorable o))
  (if-let (it (system-source-file s)) (list it)))

;;; compile-op
(defmethod operation-description ((o compile-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<compiling ~3i~_~A~@:>") c))
(defmethod operation-description ((o compile-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<completing compilation for ~3i~_~A~@:>") c))
(defgeneric* call-with-around-compile-hook (component thunk))
(defmethod call-with-around-compile-hook ((c component) function)
  (call-around-hook (around-compile-hook c) function))
(defun* perform-lisp-compilation (o c)
  (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
        ;; we consult input-files, the first of which should be the one to compile-file
        (input-file (first (input-files o c)))
        ;; on some implementations, there are more than one output-file,
        ;; but the first one should always be the primary fasl that gets loaded.
        (outputs (output-files o c)))
    (multiple-value-bind (output warnings-p failure-p)
        (destructuring-bind
            (output-file
             &optional
               #+clisp lib-file
               #+(or ecl mkcl) object-file
               #+(or clozure sbcl) warnings-file) outputs
          (call-with-around-compile-hook
           c #'(lambda (&rest flags)
                 (with-muffled-compiler-conditions ()
                   (apply 'compile-file* input-file
                          :output-file output-file
                          :external-format (component-external-format c)
                      (append
                       #+clisp (list :lib-file lib-file)
                       #+(or ecl mkcl) (list :object-file object-file)
                       #+(or clozure sbcl) (list :warnings-file warnings-file)
                       flags (compile-op-flags o)))))))
      (check-lisp-compile-results output warnings-p failure-p
                                  "~/asdf-action::format-action/" (list (cons o c))))))

(defun* report-file-p (f)
  (equal (pathname-type f) "build-report"))
(defun* perform-lisp-warnings-check (o c)
  (check-deferred-warnings
   (remove-if-not #'warnings-file-p (input-files o c))
   "~/asdf-action::format-action/" (list (cons o c)))
  (let* ((output (output-files o c))
         (report (find-if #'report-file-p output)))
    (when report
      (with-open-file (s report :direction :output :if-exists :supersede)
        (format s ":success~%")))))
(defmethod perform ((o compile-op) (c cl-source-file))
  (perform-lisp-compilation o c))
(defmethod output-files ((o compile-op) (c cl-source-file))
  (declare (ignorable o))
  (let* ((i (first (input-files o c)))
         (f (compile-file-pathname
             i #+mkcl :fasl-p #+mkcl t #+ecl :type #+ecl :fasl)))
    `(,f ;; the fasl is the primary output, in first position
      #+clisp
      ,@`(,(make-pathname :type "lib" :defaults f))
      #+(or clozure sbcl)
      ,@(let ((s (component-system c)))
          (unless (builtin-system-p s) ; includes ASDF itself
            `(,(make-pathname :type (warnings-file-type) :defaults f))))
      #+ecl
      ,@(unless (use-ecl-byte-compiler-p)
          `(,(compile-file-pathname i :type :object)))
      #+mkcl
      ,(compile-file-pathname i :fasl-p nil)))) ;; object file
(defmethod component-depends-on ((o compile-op) (c component))
  (declare (ignorable o))
  `((prepare-op ,c) ,@(call-next-method)))
(defmethod perform ((o compile-op) (c static-file))
  (declare (ignorable o c))
  nil)
(defmethod output-files ((o compile-op) (c static-file))
  (declare (ignorable o c))
  nil)
(defmethod perform ((o compile-op) (c system))
  (declare (ignorable o c))
  #+(or clozure sbcl) (perform-lisp-warnings-check o c))
#+(or clozure sbcl)
(defmethod input-files ((o compile-op) (c system))
  (declare (ignorable o c))
  (unless (builtin-system-p c)
    (loop* :for (sub-o . sub-c)
           :in (traverse-sub-actions
                o c :other-systems nil
                    :keep-operation 'compile-op :keep-component 'cl-source-file)
           :append (remove-if-not 'warnings-file-p
                                  (output-files sub-o sub-c)))))
#+sbcl
(defmethod output-files ((o compile-op) (c system))
  (unless (builtin-system-p c)
    (if-let ((pathname (component-pathname c)))
      (list (subpathname pathname (component-name c) :type "build-report")))))

;;; load-op
(defmethod operation-description ((o load-op) (c cl-source-file))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading FASL for ~3i~_~A~@:>") c))
(defmethod operation-description ((o load-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<completing load for ~3i~_~A~@:>") c))
(defmethod operation-description ((o load-op) component)
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading ~3i~_~A~@:>")
          component))
(defmethod perform-with-restarts ((o load-op) (c cl-source-file))
  (loop
    (restart-case
        (return (call-next-method))
      (try-recompiling ()
        :report (lambda (s)
                  (format s "Recompile ~a and try loading it again"
                          (component-name c)))
        (perform (find-operation o 'compile-op) c)))))
(defun* perform-lisp-load-fasl (o c)
  (if-let (fasl (first (input-files o c)))
    (with-muffled-loader-conditions () (load* fasl))))
(defmethod perform ((o load-op) (c cl-source-file))
  (perform-lisp-load-fasl o c))
(defmethod perform ((o load-op) (c static-file))
  (declare (ignorable o c))
  nil)
(defmethod component-depends-on ((o load-op) (c component))
  (declare (ignorable o))
  ;; NB: even though compile-op depends-on on prepare-op,
  ;; it is not needed-in-image-p, whereas prepare-op is,
  ;; so better not omit prepare-op and think it will happen.
  `((prepare-op ,c) (compile-op ,c) ,@(call-next-method)))


;;;; prepare-source-op, load-source-op

;;; prepare-source-op
(defmethod operation-description ((o prepare-source-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading source for dependencies of ~3i~_~A~@:>") c))
(defmethod input-files ((o prepare-source-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-source-op) (s system))
  (declare (ignorable o))
  (if-let (it (system-source-file s)) (list it)))
(defmethod perform ((o prepare-source-op) (c component))
  (declare (ignorable o c))
  nil)

;;; load-source-op
(defmethod operation-description ((o load-source-op) c)
  (declare (ignorable o))
  (format nil (compatfmt "~@<Loading source of ~3i~_~A~@:>") c))
(defmethod operation-description ((o load-source-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<Loaded source of ~3i~_~A~@:>") c))
(defmethod component-depends-on ((o load-source-op) (c component))
  (declare (ignorable o))
  `((prepare-source-op ,c) ,@(call-next-method)))
(defun* perform-lisp-load-source (o c)
  (call-with-around-compile-hook
   c #'(lambda ()
         (with-muffled-loader-conditions ()
           (load* (first (input-files o c))
                  :external-format (component-external-format c))))))

(defmethod perform ((o load-source-op) (c cl-source-file))
  (perform-lisp-load-source o c))
(defmethod perform ((o load-source-op) (c static-file))
  (declare (ignorable o c))
  nil)
(defmethod output-files ((o load-source-op) (c component))
  (declare (ignorable o c))
  nil)


;;;; test-op
(defmethod perform ((o test-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod operation-done-p ((o test-op) (c system))
  "Testing a system is _never_ done."
  (declare (ignorable o c))
  nil)
(defmethod component-depends-on ((o test-op) (c system))
  (declare (ignorable o))
  `((load-op ,c) ,@(call-next-method)))

