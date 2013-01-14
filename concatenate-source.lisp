;;;; -------------------------------------------------------------------------
;;;; Concatenate-source

(asdf/package:define-package :asdf/concatenate-source
  (:recycle :asdf/concatenate-source :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/operation
   :asdf/system :asdf/find-system :asdf/defsystem
   :asdf/action :asdf/lisp-action :asdf/bundle)
  (:export
   #:concatenate-source-op
   #:load-concatenated-source-op
   #:compile-concatenated-source-op
   #:load-compiled-concatenated-source-op
   #:monolithic-concatenate-source-op
   #:monolithic-load-concatenated-source-op
   #:monolithic-compile-concatenated-source-op
   #:monolithic-load-compiled-concatenated-source-op
   #:concatenated-source-system
   #:system-concatenated-source-file
   #:system-translate-output-p
   #:translate-output-p #:concatenated-source-file))   
(in-package :asdf/concatenate-source)

;;;
;;; Concatenate sources
;;;
(defclass concatenate-source-op (operation) ())
(defclass load-concatenated-source-op (basic-load-op operation) ())
(defclass compile-concatenated-source-op (basic-compile-op operation) ())
(defclass load-compiled-concatenated-source-op (basic-load-op operation) ())
(defclass monolithic-concatenate-source-op (concatenate-source-op monolithic-op) ())
(defclass monolithic-load-concatenated-source-op (load-concatenated-source-op monolithic-op) ())
(defclass monolithic-compile-concatenated-source-op (compile-concatenated-source-op monolithic-op) ())
(defclass monolithic-load-compiled-concatenated-source-op (load-compiled-concatenated-source-op monolithic-op) ())

(defclass concatenated-source-system (system)
  ((concatenated-source-file
    :initform nil :initarg :concatenated-source-file :reader system-concatenated-source-file)
   (translate-output-p
    :initform t :initarg :translate-output-p :reader system-translate-output-p)))

(defmethod system-concatenated-source-file ((s system))
  (declare (ignorable s))
  nil)
(defmethod system-translate-output-p ((s system))
  (declare (ignorable s))
  t)

(defmethod input-files ((o concatenate-source-op) (s system))
  (loop :with op = (make-operation 'compile-op)
        :with components = (gather-components op s
                                              :include-self nil
                                              :filter-type 'source-file
                                              :other-systems (operation-monolithic-p o))
        :with non-cl-source-files = nil
        :with encoding = (or (component-encoding s) *default-encoding*)
        :with other-encodings = '()
        :with around-compile = (around-compile-hook s)
        :with other-around-compile = '()
        :for (o . c) :in components
        :do (cond
              ((typep c 'cl-source-file)
               (let ((e (component-encoding c)))
                 (unless (equal e encoding)
                   (pushnew e other-encodings :test 'equal)))
               (let ((a (around-compile-hook c)))
                 (unless (equal a around-compile)
                   (pushnew a other-around-compile :test 'equal))))
              (t
               (push c non-cl-source-files)))
        :append (input-files o c) :into inputs
        :finally
           (when non-cl-source-files
             (warn "~S depends on these non CL source files: ~A"
                   'concatenated-source-op non-cl-source-files))
           (when other-encodings
             (warn "~S uses encoding ~A but has sources that use these encodings: ~A"
                   'concatenated-source-op encoding other-encodings))
           (when other-around-compile
             (warn "~S uses around-compile hook ~A but has sources that use these hooks: ~A"
                   'concatenated-source-op around-compile other-around-compile))
           (return inputs)))

(defmethod output-files ((o concatenate-source-op) (s system))
  (declare (ignorable o))
  (let ((of (or (system-concatenated-source-file s) (strcat (coerce-name s) ".lisp")))
        (top (system-translate-output-p s)))
    (values (list (system-relative-pathname s of)) (not top))))
(defmethod input-files ((o load-concatenated-source-op) (s system))
  (output-files (find-operation o 'concatenate-source-op) s))
(defmethod input-files ((o compile-concatenated-source-op) (s system))
  (output-files (find-operation o 'concatenate-source-op) s))
(defmethod output-files ((o compile-concatenated-source-op) (s system))
  (let ((input (first (input-files o s))))
    (list (compile-file-pathname input))))
(defmethod input-files ((o load-compiled-concatenated-source-op) (s system))
  (output-files (find-operation o 'compile-concatenated-source-op) s))

(defmethod perform ((o concatenate-source-op) (s system))
  (let ((inputs (input-files o s))
        (output (output-file o s)))
    (concatenate-files inputs output)))
(defmethod perform ((o load-concatenated-source-op) (s system))
  (perform-lisp-load-source o s))
(defmethod perform ((o compile-concatenated-source-op) (s system))
  (perform-lisp-compilation o s))
(defmethod perform ((o load-compiled-concatenated-source-op) (s system))
  (perform-lisp-load-fasl o s))

(defmethod component-depends-on ((o concatenate-source-op) (s system))
  (declare (ignorable o s)) nil)
(defmethod component-depends-on ((o load-concatenated-source-op) (s system))
  (declare (ignorable o s)) `((prepare-op ,s) (concatenate-source-op ,s)))
(defmethod component-depends-on ((o compile-concatenated-source-op) (s system))
  (declare (ignorable o s)) `((concatenate-source-op ,s)))
(defmethod component-depends-on ((o load-compiled-concatenated-source-op) (s system))
  (declare (ignorable o s)) `((compile-concatenated-source-op ,s)))

(defmethod component-depends-on ((o monolithic-concatenate-source-op) (s system))
  (declare (ignorable o s)) nil)
(defmethod component-depends-on ((o monolithic-load-concatenated-source-op) (s system))
  (declare (ignorable o s)) `((monolithic-concatenate-source-op ,s)))
(defmethod component-depends-on ((o monolithic-compile-concatenated-source-op) (s system))
  (declare (ignorable o s)) `((monolithic-concatenate-source-op ,s)))
(defmethod component-depends-on ((o monolithic-load-compiled-concatenated-source-op) (s system))
  (declare (ignorable o s)) `((monolithic-compile-concatenated-source-op ,s)))

