;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(asdf/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate)
  (:export
   #:bundle-op #:bundle-op-build-args
   #:fasl-op #:load-fasl-op #:lib-op #:dll-op #:binary-op
   #:monolithic-op #:monolithic-bundle-op
   #:monolithic-binary-op #:monolithic-fasl-op #:monolithic-lib-op #:monolithic-dll-op
   #:program-op #:compiled-file #:precompiled-system #:prebuild-system
   #:operation-monolithic-p
   #:user-system-p #:user-system #:trivial-system-p
   #:bundle-sub-operations #:gather-components
   #+ecl #:make-build
   #+mkcl #:mkcl-bundle-sub-operations #+mkcl #:files-to-bundle #+mkcl #:bundle-system
   #+(or ecl mkcl) #:register-pre-built-system
   #:build-args #:name-suffix #:prologue-code #:epilogue-code #:static-library
   #:system-fasl))
(in-package :asdf/bundle)

(defclass bundle-op (operation)
  ((build-args :initarg :args :initform nil :accessor bundle-op-build-args)
   (name-suffix :initarg :name-suffix :initform nil)
   #+ecl (type :reader bundle-op-type)
   #+ecl (lisp-files :initform nil :accessor bundle-op-lisp-files)
   #+mkcl (do-fasb :initarg :do-fasb :initform t :reader bundle-op-do-fasb-p)
   #+mkcl (do-static-library :initarg :do-static-library :initform t :reader bundle-op-do-static-library-p)))

(defclass fasl-op (bundle-op)
  ((type :initform :fasl)))

(defclass load-fasl-op (basic-load-op) ())

(defclass lib-op (bundle-op)
  ((type :initform :lib)))

(defclass dll-op (bundle-op)
  ((type :initform :dll)))

(defclass binary-op (bundle-op)
  ())

(defclass monolithic-op (operation) ()) ;; operation on a system and its dependencies

(defclass monolithic-bundle-op (monolithic-op bundle-op)
  ((prologue-code :accessor monolithic-op-prologue-code)
   (epilogue-code :accessor monolithic-op-epilogue-code)))

(defclass monolithic-binary-op (binary-op monolithic-bundle-op)
  ())

(defclass monolithic-fasl-op (monolithic-bundle-op fasl-op) ())

(defclass monolithic-lib-op (monolithic-bundle-op lib-op)
  ((type :initform :lib)))

(defclass monolithic-dll-op (monolithic-bundle-op dll-op)
  ((type :initform :dll)))

(defclass program-op (monolithic-bundle-op)
  ((type :initform :program)))

(defclass compiled-file (file-component)
  ((type :initform #-(or ecl mkcl) (fasl-type) #+(or ecl mkcl) "fasb")))

(defclass precompiled-system (system)
  ((fasl :initarg :fasl :reader %system-fasl)))

(defclass prebuilt-system (system)
  ((static-library :accessor prebuilt-system-static-library :initarg :lib)))

;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;

(defun* operation-monolithic-p (op)
  (typep op 'monolithic-op))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                       &key (name-suffix nil name-suffix-p)
                                       &allow-other-keys)
  (declare (ignorable initargs name-suffix))
  (format t "IIBO a ~S with ~S" (type-of instance) initargs)
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
          (if (operation-monolithic-p instance) ".all-systems" ".system")))
  (when (typep instance 'monolithic-bundle-op)
    (destructuring-bind (&rest original-initargs
                         &key lisp-files prologue-code epilogue-code
                         &allow-other-keys)
        (operation-original-initargs instance)
      (setf (operation-original-initargs instance)
            (remove-keys '(lisp-files epilogue-code prologue-code) original-initargs)
            (monolithic-op-prologue-code instance) prologue-code
            (monolithic-op-epilogue-code instance) epilogue-code)
      #-ecl (assert (null lisp-files))
      #+ecl (setf (bundle-op-lisp-files instance) lisp-files)))
  (setf (bundle-op-build-args instance)
        (remove-keys '(type monolithic name-suffix)
                     (operation-original-initargs instance))))

(defmethod bundle-op-build-args :around ((o lib-op))
  (declare (ignorable o))
  (let ((args (call-next-method)))
    (remf args :ld-flags)
    args))

(defun* gather-components (operation system
                                     &key other-systems filter-type include-self)
  ;; This function creates a list of actions pairing the operation with sub-components of system
  ;; and its dependencies if requested.
  ;; This list may be restricted to sub-components of SYSTEM
  ;; if GATHER-ALL = NIL (default), and it may include the system itself.
  (let ((tree (traverse-sequentially (make-operation 'load-op) system
                                     :force (if other-systems :all t)
                                     :force-not (if other-systems nil :all))))
    `(,@(loop :for (op . component) :in tree
              :when (and (typep op 'load-op)
                         (typep component filter-type))
                :collect (progn
                           (when (eq component system) (setf include-self nil))
                           `(,operation . ,component)))
      ,@(and include-self `((,operation . ,system))))))

(defgeneric* trivial-system-p (component))

(defun* user-system-p (s)
  (and (typep s 'system)
       (not (builtin-system-p s))
       (not (trivial-system-p s))))

(deftype user-system () '(and system (satisfies user-system-p)))

;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component)
;;; which contains all the dependencies of this bundle.
;;; This list is used by TRAVERSE and also by INPUT-FILES.
;;; The dependencies depend on the strategy, as explained below.
;;;
(defgeneric* bundle-sub-operations (operation component))
;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
;;; Gather the static libraries of all components.
;;;
(defmethod bundle-sub-operations ((o monolithic-bundle-op) c)
  (gather-components (find-operation o 'lib-op) c :filter-type 'user-system :include-self t))

;;;
;;; STATIC LIBRARIES
;;;
;;; Gather the object files of all components
;;; and, if monolithic, also of systems and subsystems.
;;;
(defmethod bundle-sub-operations ((o lib-op) c)
  (gather-components (find-operation o 'compile-op) c
                     :other-systems (operation-monolithic-p o)
                     :filter-type '(not system)))
;;;
;;; SHARED LIBRARIES
;;;
;;; Gather the dynamically linked libraries of all components.
;;; They will be linked into this new shared library,
;;; together with the static library of this module.
;;;
(defmethod bundle-sub-operations ((o dll-op) c)
  `((,(find-operation o 'lib-op) . ,c)))
;;;
;;; FASL FILES
;;;
;;; Gather the statically linked library of this component.
;;;
(defmethod bundle-sub-operations ((o fasl-op) c)
  `((,(find-operation o 'lib-op) . ,c)))

#-mkcl
(defmethod component-depends-on ((o bundle-op) (c system))
  `(,@(loop :for (op . dep) :in (bundle-sub-operations o c)
            :when (user-system-p dep) :collect (list op dep))
    ,@(call-next-method)))

(defmethod component-depends-on ((o lib-op) (c system))
  (declare (ignorable o))
  `((compile-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((o bundle-op) c)
  (declare (ignorable o c))
  nil)

#-mkcl
(defmethod input-files ((o bundle-op) (c system))
  (loop :for (sub-op . sub-c) :in (bundle-sub-operations o c)
        :nconc (output-files sub-op sub-c)))

#-mkcl
(defmethod output-files ((o bundle-op) (c system))
  (list (compile-file-pathname
         (make-pathname
          :name (strcat (component-name c) (slot-value o 'name-suffix)
                        #|"-" (string-downcase (implementation-type))|#)
          :type "lisp"
          :defaults (system-source-directory c))
         #+ecl :type #+ecl (bundle-op-type o))))

(defun* select-operation (monolithic type)
  (ecase type
    ((:binary)
     (if monolithic 'monolithic-binary-op 'binary-op))
    ((:dll :shared-library)
     (if monolithic 'monolithic-dll-op 'dll-op))
    ((:lib :static-library)
     (if monolithic 'monolithic-lib-op 'lib-op))
    ((:fasl)
     (if monolithic 'monolithic-fasl-op 'fasl-op))
    ((:program)
     'program-op)))

(defun* make-build (system &rest args &key (monolithic nil) (type :fasl)
                   (move-here nil move-here-p)
                   &allow-other-keys)
  (let* ((operation-name (select-operation monolithic type))
         (move-here-path (if (and move-here
                                  (typep move-here '(or pathname string)))
                             (pathname move-here)
                             (merge-pathnames "./asdf-output/")))
         (operation (apply #'operate operation-name
                           system
                           (remove-keys '(monolithic type move-here) args)))
         (system (find-system system))
         (files (and system (output-files operation system))))
    (if (or move-here (and (null move-here-p)
                           (member operation-name '(:program :binary))))
        (loop :with dest-path = (truename* (ensure-directories-exist move-here-path))
              :for f :in files
              :for new-f = (make-pathname :name (pathname-name f)
                                :type (pathname-type f)
                                :defaults dest-path)
              :do (rename-file-overwriting-target f new-f)
              :collect new-f)
        files)))

;;;
;;; LOAD-FASL-OP
;;;
;;; This is like ASDF's LOAD-OP, but using monolithic fasl files.
;;;

(defmethod component-depends-on ((o load-fasl-op) (c system))
  (declare (ignorable o))
  `((load-fasl-op ,@(loop :for dep :in (component-sibling-dependencies c)
                     :collect (resolve-dependency-spec c dep)))
    (,(if (user-system-p c) 'fasl-op 'load-op) ,c)
    ,@(call-next-method)))

(defmethod input-files ((o load-fasl-op) (c system))
  (when (user-system-p c)
    (output-files (find-operation o 'fasl-op) c)))

(defmethod perform ((o load-fasl-op) c)
  (declare (ignorable o c))
  nil)

(defmethod perform ((o load-fasl-op) (c system))
  (perform-lisp-load-fasl o c))

(defmethod mark-operation-done :after ((o load-fasl-op) (c system))
  (mark-operation-done (find-operation o 'load-op) c)) ; need we recurse on gather-components?

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF systems in precompiled form.
;;; Only useful when the dependencies have also been precompiled.
;;;

(defmethod trivial-system-p ((s system))
  (every #'(lambda (c) (typep c 'compiled-file)) (component-children s)))

(defmethod output-files (o (c compiled-file))
  (declare (ignorable o c))
  nil)
(defmethod input-files (o (c compiled-file))
  (declare (ignorable o))
  (component-pathname c))
(defmethod perform ((o load-op) (c compiled-file))
  (perform-lisp-load-fasl o c))
(defmethod perform ((o load-source-op) (c compiled-file))
  (perform (find-operation o 'load-op) c))
(defmethod perform ((o load-fasl-op) (c compiled-file))
  (perform (find-operation o 'load-op) c))
(defmethod perform (o (c compiled-file))
  (declare (ignorable o c))
  nil)

;;;
;;; Pre-built systems
;;;
(defmethod trivial-system-p ((s prebuilt-system))
  (declare (ignorable s))
  t)

(defmethod output-files ((o lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (values (list (prebuilt-system-static-library c))
          t)) ; Advertise that we do not want this path renamed by asdf-output-translations

(defmethod perform ((o lib-op) (c prebuilt-system))
  (first (output-files o c)))

(defmethod component-depends-on ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o monolithic-lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (error "Prebuilt system ~S shipped with ECL can not be used in a monolithic library operation." c))

(defmethod bundle-sub-operations ((o monolithic-bundle-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

;;;
;;; PREBUILT SYSTEM CREATOR
;;;

(defun* binary-op-dependencies (o s)
  (multiple-value-bind (lib-op fasl-op)
      (if (operation-monolithic-p o)
          (values 'monolithic-lib-op 'monolithic-fasl-op)
          (values 'lib-op 'fasl-op))
    `((,(find-operation o lib-op) ,s)
      (,(find-operation o fasl-op) ,s))))

(defmethod component-depends-on ((o binary-op) (s system))
  `(,@(loop :for dep :in (binary-op-dependencies o s)
            :append (apply #'component-depends-on dep))
    ,@(call-next-method)))

(defmethod input-files ((o binary-op) (s system))
  (loop :for dep :in (binary-op-dependencies o s)
        :append (apply #'input-files dep)))

(defmethod output-files ((o binary-op) (s system))
  (list* (merge-pathnames* (make-pathname :name (component-name s)
                                          :type "asd")
                           (component-relative-pathname s))
         (loop :for dep :in (binary-op-dependencies o s)
               :append (apply #'output-files dep))))

(defmethod perform ((o binary-op) (s system))
  (let* ((dependencies (binary-op-dependencies o s))
         (library (first (apply #'output-files (first dependencies))))
         (fasl (first (apply #'output-files (second dependencies))))
         (filename (first (output-files o s)))
         (name (component-name s))
         (name-keyword (intern (string name) (find-package :keyword))))
    (dolist (dep dependencies)
      (apply #'perform dep))
    (with-open-file (s filename :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (format s ";;; Prebuilt ASDF definition for system ~A" name)
      (format s ";;; Built for ~A ~A on a ~A/~A ~A"
              (lisp-implementation-type)
              (lisp-implementation-version)
              (software-type)
              (machine-type)
              (software-version))
      (let ((*package* (find-package :keyword)))
        (pprint `(defsystem ,name-keyword
                     :class prebuilt-system
                     :components ((:compiled-file ,(pathname-name fasl)))
                     :lib ,(make-pathname :name (pathname-name library)
                                          :type (pathname-type library)))
                s)))))

#-(or ecl mkcl)
(defmethod perform ((o bundle-op) (c system))
  (let* ((input-files (input-files o c))
         (fasl-files (remove (fasl-type) input-files :key #'pathname-type :test-not #'string=))
         (non-fasl-files (remove (fasl-type) input-files :key #'pathname-type :test #'string=))
         (output-files (output-files o c))
         (output-file (first output-files)))
    (when input-files
      (assert output-files)
      (when non-fasl-files
        (error "On ~A, asdf-bundle can only bundle FASL files, but these were also produced: ~S"
               (implementation-type) non-fasl-files))
      (when (and (typep o 'monolithic-bundle-op)
                 (or (monolithic-op-prologue-code o) (monolithic-op-epilogue-code o)))
        (error "prologue-code and epilogue-code are not supported on ~A"
               (implementation-type)))
      (with-staging-pathname (output-file)
        (combine-fasls fasl-files output-file)))))

(defmethod output-files ((o fasl-op) (c source-file))
  (declare (ignorable o c))
  nil)

(defmethod input-files ((o fasl-op) (c source-file))
  (declare (ignorable o c))
  nil)

(defgeneric* system-fasl (system))
(defmethod system-fasl ((system precompiled-system))
  (let* ((f (%system-fasl system))
         (p (etypecase f
              ((or pathname string) f)
              (function (funcall f))
              (cons (eval f)))))
    (pathname p)))

(defmethod input-files ((o load-op) (s precompiled-system))
  (declare (ignorable o))
  (list (system-fasl s)))

(defmethod component-depends-on ((o load-fasl-op) (s precompiled-system))
  (declare (ignorable o))
  `((load-op ,s) ,@(call-next-method)))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+ecl
(defmethod output-files ((o fasl-op) (c system))
  (declare (ignorable o c))
  (loop :for file :in (call-next-method)
        :collect (make-pathname :type "fasb" :defaults file)))

#+ecl
(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (remove "fas" (input-files o c)
                               :key #'pathname-type :test #'string=))
         (output (output-files o c)))
    (apply #'c::builder (bundle-op-type o) (first output)
           :lisp-files (append object-files (bundle-op-lisp-files o))
           (append (bundle-op-build-args o)
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-prologue-code o))
                     `(:prologue-code ,(monolithic-op-prologue-code o)))
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-epilogue-code o))
                     `(:epilogue-code ,(monolithic-op-epilogue-code o)))))))

#+mkcl
(progn
;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component) which contains all the
;;; dependencies of this bundle.
;;;

(defun* mkcl-bundle-sub-operations (op sys)
  (gather-components (find-operation op 'compile-op) sys
                     :other-systems nil
                     :filter-type '(not system)))

(defun* files-to-bundle (operation system)
  (loop :for (o . c) :in (mkcl-bundle-sub-operations operation system)
    :for sub-files = (output-files o c)
    :when sub-files :collect (first sub-files)))

(defmethod component-depends-on ((o bundle-op) (c system))
  (declare (ignorable o))
  `((compile-op ,c) ,@(call-next-method)))

(defmethod output-files ((o bundle-op) (c system))
  (let* ((name (component-name c))
         (static-lib-name (merge-pathnames
                           (compiler::builder-internal-pathname name :static-library)
                           (component-relative-pathname c)))
         (fasl-bundle-name (merge-pathnames
                            (compiler::builder-internal-pathname name :fasb)
                            (component-relative-pathname c))))
    (list static-lib-name fasl-bundle-name)))

(defmethod perform ((o bundle-op) (c system))
  (let ((object-files (files-to-bundle o c))
        (output (output-files o c)))
    (when (bundle-op-do-static-library-p o)
      (apply #'compiler::build-static-library (first output)
             :lisp-object-files object-files (bundle-op-build-args o)))
    (when (bundle-op-do-fasb-p o)
      (apply #'compiler::build-bundle (second output)
             :lisp-object-files object-files (bundle-op-build-args o)))))

(defun* bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
  (declare (ignore force verbose version))
  (apply #'operate 'bundle-op system args))
);mkcl

#+(or ecl mkcl)
(defun* register-pre-built-system (name)
  (register-system (make-instance 'system :name (coerce-name name) :source-file nil)))
