;;;; -------------------------------------------------------------------------
;;;; Actions to build Common Lisp software

(asdf/package:define-package :asdf/lisp-action
  (:recycle :asdf/lisp-action :asdf)
  (:intern #:proclamations #:flags)
  (:use :common-lisp :asdf/utility :asdf/lisp-build
   :asdf/component :asdf/system :asdf/find-component :asdf/operation :asdf/action)
  (:export
   #:compile-error #:compile-failed #:compile-warned #:try-recompiling
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:basic-load-op #:basic-compile-op
   #:load-op #:prepare-op #:compile-op #:test-op #:load-source-op #:prepare-source-op
   #:call-with-around-compile-hook
   #:perform-lisp-compilation #:perform-lisp-load-fasl #:perform-lisp-load-source))
(in-package :asdf/lisp-action)

;;;; Conditions

(define-condition compile-error (operation-error) ())
(define-condition compile-failed (compile-error) ())
(define-condition compile-warned (compile-error) ())


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
(defclass load-op (basic-load-op downward-operation) ())
(defclass prepare-op (upward-operation) ())
(defclass compile-op (basic-compile-op downward-operation) ())

(defclass load-source-op (basic-load-op downward-operation) ())
(defclass prepare-source-op (upward-operation) ())

(defclass test-op (operation) ())


;;;; prepare-op, compile-op and load-op

;;; prepare-op
(defmethod operation-description ((o prepare-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading dependencies of ~3i~_~A~@:>") c))
(defmethod component-depends-on ((o prepare-op) (c component))
  (declare (ignorable o))
  `((load-op ,@(loop :for dep :in (component-sibling-dependencies c)
                     :collect (resolve-dependency-spec c dep)))
    ,@(call-next-method)))
(defmethod perform ((o prepare-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-op) (s system))
  (declare (ignorable o))
  (aif (system-source-file s) (list it)))

;;; compile-op
(defmethod operation-description ((o compile-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<compiling ~3i~_~A~@:>") c))
(defmethod operation-description ((o compile-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<compiled ~3i~_~A~@:>") c))
(defgeneric* call-with-around-compile-hook (component thunk))
(defmethod call-with-around-compile-hook ((c component) function)
  (call-around-hook (around-compile-hook c) function))
(defun* perform-lisp-compilation (o c)
  (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
        ;; we consult input-files, the first of which should be the one to compile-file
        (input-file (first (input-files o c)))
        ;; on some implementations, there are more than one output-file,
        ;; but the first one should always be the primary fasl that gets loaded.
        (output-file (first (output-files o c))))
    (multiple-value-bind (output warnings-p failure-p)
        (call-with-around-compile-hook
         c #'(lambda (&rest flags)
               (apply *compile-file-function* input-file
                      :output-file output-file
                      #-gcl<2.7 :external-format #-gcl<2.7 (component-external-format c)
                      (append flags (compile-op-flags o)))))
      (unless output
        (error 'compile-error :component c :operation o))
      (when failure-p
        (case *compile-file-failure-behaviour*
          (:warn (warn
                  (compatfmt "~@<COMPILE-FILE failed while performing ~A.~@:>")
                  (operation-description o c)))
          (:error (error 'compile-failed :component c :operation o))
          (:ignore nil)))
      (when warnings-p
        (case *compile-file-warnings-behaviour*
          (:warn (warn
                  (compatfmt "~@<COMPILE-FILE warned while performing ~A.~@:>")
                  (operation-description o c)))
          (:error (error 'compile-warned :component c :operation o))
          (:ignore nil))))))
(defmethod perform ((o compile-op) (c cl-source-file))
  (perform-lisp-compilation o c))
(defmethod output-files ((o compile-op) (c cl-source-file))
  (declare (ignorable o))
  (let* ((i (first (input-files o c)))
         (f (compile-file-pathname
             i #+mkcl :fasl-p #+mkcl t #+ecl :type #+ecl :fasl))
         #+mkcl (o (compile-file-pathname i :fasl-p nil))) ;; object file
    #+ecl (if (use-ecl-byte-compiler-p)
              (list f)
              (list f (compile-file-pathname i :type :object)))
    #+mkcl (list f o)
    #-(or ecl mkcl) (list f)))
(defmethod component-depends-on ((o compile-op) (c component))
  (declare (ignorable o))
  `((prepare-op ,c) ,@(call-next-method)))
(defmethod perform ((o compile-op) (c static-file))
  (declare (ignorable o c))
  nil)
(defmethod output-files ((o compile-op) (c static-file))
  (declare (ignorable o c))
  nil)

;;; load-op
(defmethod operation-description ((o load-op) (c cl-source-file))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading FASL for ~3i~_~A~@:>") c))
(defmethod operation-description ((o load-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loaded ~3i~_~A~@:>") c))
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
(defun perform-lisp-load-fasl (o c)
  (load (first (input-files o c))))
(defmethod perform ((o load-op) (c cl-source-file))
  (perform-lisp-load-fasl o c))
(defmethod perform ((o load-op) (c static-file))
  (declare (ignorable o c))
  nil)
(defmethod component-depends-on ((o load-op) (c component))
  (declare (ignorable o))
  `((prepare-op ,c) ,@(call-next-method)))
(defmethod component-depends-on ((o load-op) (c source-file))
  (declare (ignorable o))
  `((compile-op ,c) ,@(call-next-method)))


;;;; prepare-source-op, load-source-op

;;; prepare-source-op
(defmethod operation-description ((o prepare-source-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading source for dependencies of ~3i~_~A~@:>") c))
(defmethod component-depends-on ((o prepare-source-op) (c source-file))
  (declare (ignorable o))
  `((load-source-op ,@(component-sibling-dependencies c)) ,@(call-next-method)))
(defmethod input-files ((o prepare-source-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-source-op) (s system))
  (declare (ignorable o))
  (aif (system-source-file s) (list it)))
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
(defun perform-lisp-load-source (o c)
  (call-with-around-compile-hook
   c #'(lambda () (load (first (input-files o c))
                        #-gcl<2.7 :external-format #-gcl<2.7 (component-external-format c)))))
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

