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
   #:component-concatenated-source-file
   #:concatenated-source-file))
(in-package :asdf/concatenate-source)

;;;
;;; Concatenate sources
;;;
(defclass concatenate-source-op (bundle-op)
  ((bundle-type :initform "lisp")))
(defclass load-concatenated-source-op (basic-load-op operation)
  ((bundle-type :initform :no-output-file)))
(defclass compile-concatenated-source-op (basic-compile-op bundle-op)
  ((bundle-type :initform :fasl)))
(defclass load-compiled-concatenated-source-op (basic-load-op operation)
  ((bundle-type :initform :no-output-file)))

(defclass monolithic-concatenate-source-op (concatenate-source-op monolithic-op) ())
(defclass monolithic-load-concatenated-source-op (load-concatenated-source-op monolithic-op) ())
(defclass monolithic-compile-concatenated-source-op (compile-concatenated-source-op monolithic-op) ())
(defclass monolithic-load-compiled-concatenated-source-op (load-compiled-concatenated-source-op monolithic-op) ())

(defclass concatenated-source-system (bundle-system)
  ((bundle-pathname :initarg :concatenated-source-file)
   (bundle-operation :initform :load-compiled-concatenated-source-op)))

(defmethod input-files ((operation concatenate-source-op) (s system))
  (loop :with encoding = (or (component-encoding s) *default-encoding*)
        :with other-encodings = '()
        :with around-compile = (around-compile-hook s)
        :with other-around-compile = '()
        :for c :in (gather-components s
                                      :goal-operation 'compile-op
                                      :keep-operation 'compile-op
                                      :component-type 'component
                                      :other-systems (operation-monolithic-p operation))
        :append
        (when (typep c 'cl-source-file)
          (let ((e (component-encoding c)))
            (unless (equal e encoding)
              (pushnew e other-encodings :test 'equal)))
          (let ((a (around-compile-hook c)))
            (unless (equal a around-compile)
              (pushnew a other-around-compile :test 'equal)))
          (input-files (make-operation 'compile-op) c)) :into inputs
        :finally
           (when other-encodings
             (warn "~S uses encoding ~A but has sources that use these encodings: ~A"
                   operation encoding other-encodings))
           (when other-around-compile
             (warn "~S uses around-compile hook ~A but has sources that use these hooks: ~A"
                   operation around-compile other-around-compile))
           (return inputs)))

(defmethod input-files ((o load-concatenated-source-op) (s system))
  (output-files (find-operation o 'concatenate-source-op) s))
(defmethod input-files ((o compile-concatenated-source-op) (s system))
  (output-files (find-operation o 'concatenate-source-op) s))
(defmethod output-files ((o compile-concatenated-source-op) (s system))
  (let ((input (first (input-files o s))))
    (list (compile-file-pathname input))))
(defmethod input-files ((o load-compiled-concatenated-source-op) (s system))
  (output-files (find-operation o 'compile-concatenated-source-op) s))

(defmethod input-files ((o monolithic-load-concatenated-source-op) (s system))
  (output-files (find-operation o 'monolithic-concatenate-source-op) s))
(defmethod input-files ((o monolithic-compile-concatenated-source-op) (s system))
  (output-files (find-operation o 'monolithic-concatenate-source-op) s))
(defmethod output-files ((o monolithic-compile-concatenated-source-op) (s system))
  (let ((input (first (input-files o s))))
    (list (compile-file-pathname input))))
(defmethod input-files ((o monolithic-load-compiled-concatenated-source-op) (s system))
  (output-files (find-operation o 'monolithic-compile-concatenated-source-op) s))

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

