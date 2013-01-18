;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(asdf/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate)
  (:export
   #:bundle-op #:bundle-op-build-args #:bundle-type #:bundle-system #:bundle-pathname-type
   #:fasl-op #:load-fasl-op #:lib-op #:dll-op #:binary-op
   #:monolithic-op #:monolithic-bundle-op #:dependency-files
   #:monolithic-binary-op #:monolithic-fasl-op #:monolithic-lib-op #:monolithic-dll-op
   #:program-op
   #:compiled-file #:precompiled-system #:prebuilt-system
   #:operation-monolithic-p
   #:user-system-p #:user-system #:trivial-system-p
   #:gather-actions #:operated-components
   #+ecl #:make-build #+mkcl #:bundle-system
   #:register-pre-built-system
   #:build-args #:name-suffix #:prologue-code #:epilogue-code #:static-library
   #:component-translate-output-p #:translate-output-p
   #:component-bundle-pathname #:bundle-pathname
   #:component-bundle-operation #:bundle-operation
   #:component-entry-point #:entry-point))
(in-package :asdf/bundle)

(defclass bundle-op (operation)
  ((build-args :initarg :args :initform nil :accessor bundle-op-build-args)
   (name-suffix :initarg :name-suffix :initform nil)
   (bundle-type :reader bundle-type)
   #+ecl (lisp-files :initform nil :accessor bundle-op-lisp-files)
   #+mkcl (do-fasb :initarg :do-fasb :initform t :reader bundle-op-do-fasb-p)
   #+mkcl (do-static-library :initarg :do-static-library :initform t :reader bundle-op-do-static-library-p)))

(defclass fasl-op (bundle-op)
  ;; create a single fasl for the entire library
  ((bundle-type :initform :fasl)))

(defclass load-fasl-op (basic-load-op)
  ;; load a single fasl for the entire library
  ())

(defclass lib-op (bundle-op)
  ;; On ECL: compile the system and produce linkable .a library for it.
  ;; On others: just compile the system.
  ((bundle-type :initform #+(or ecl mkcl) :lib #-(or ecl mkcl) :no-output-file)))

(defclass dll-op (bundle-op)
  ;; Link together all the dynamic library used by this system into a single one.
  ((bundle-type :initform :dll)))

(defclass binary-op (bundle-op)
  ;; On ECL: produce lib and fasl for the system.
  ;; On "normal" Lisps: produce just the fasl.
  ())

(defclass monolithic-op (operation) ()) ;; operation on a system and its dependencies

(defclass monolithic-bundle-op (monolithic-op bundle-op)
  ((prologue-code :accessor monolithic-op-prologue-code)
   (epilogue-code :accessor monolithic-op-epilogue-code)))

(defclass monolithic-binary-op (binary-op monolithic-bundle-op)
  ;; On ECL: produce lib and fasl for combined system and dependencies.
  ;; On "normal" Lisps: produce an image file from system and dependencies.
  ())

(defclass monolithic-fasl-op (monolithic-bundle-op fasl-op)
  ;; Create a single fasl for the system and its dependencies.
  ())

(defclass monolithic-lib-op (monolithic-bundle-op lib-op)
  ;; ECL: Create a single linkable library for the system and its dependencies.
  ((bundle-type :initform :lib)))

(defclass monolithic-dll-op (monolithic-bundle-op dll-op)
  ((bundle-type :initform :dll)))

(defclass program-op (monolithic-bundle-op)
  ;; All: create an executable file from the system and its dependencies
  ((bundle-type :initform :program)))

(defgeneric* component-bundle-pathname (component))
(defgeneric* component-translate-output-p (component))
(defgeneric* component-entry-point (component))

(defmethod component-bundle-pathname ((c component))
  (declare (ignorable c))
  nil)
(defmethod component-translate-output-p ((c component))
  (declare (ignorable c))
  t)
(defmethod component-entry-point ((c component))
  (declare (ignorable c))
  nil)

(defclass bundle-system (system)
  ((bundle-pathname
    :initform nil :initarg :bundle-pathname :accessor component-bundle-pathname)
   (bundle-operation
    :initarg :bundle-operation :accessor component-bundle-operation)
   (entry-point
    :initform nil :initarg :entry-point :accessor component-entry-point)
   (translate-output-p
    :initform nil :initarg :translate-output-p :accessor component-translate-output-p)))

(defun* bundle-pathname-type (bundle-type)
  (etypecase bundle-type
    ((eql :no-output-file) nil) ;; should we error out instead?    
    ((or null string) bundle-type)
    ((eql :fasl) #-(or ecl mkcl) (compile-file-type) #+(or ecl mkcl) "fasb")
    #+ecl
    ((member :binary :dll :lib :static-library :program :object :program)
     (compile-file-type :type bundle-type))
    ((eql :binary) "image")
    ((eql :dll) (cond ((os-unix-p) "so") ((os-windows-p) "dll")))
    ((member :lib :static-library) (cond ((os-unix-p) "a") ((os-windows-p) "lib")))
    ((eql :program) (cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

(defun* bundle-output-files (o c)
  (let ((bundle-type (bundle-type o)))
    (unless (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
      (let ((name (or (component-bundle-pathname c)
                      (format nil "~A~@[~A~]" (component-name c) (slot-value o 'name-suffix))))
            (type (bundle-pathname-type bundle-type)))
        (values (list (subpathname (component-pathname c) name :type type))
                (not (component-translate-output-p c)))))))

(defmethod output-files ((o bundle-op) (c system))
  (bundle-output-files o c))

#-(or ecl mkcl)
(progn
  (defmethod perform ((o program-op) (c system))
    (let ((output-file (output-file o c)))
      (setf *image-entry-point* (ensure-function (component-entry-point c)))
      (dump-image output-file :executable t)))

  (defmethod perform ((o monolithic-binary-op) (c system))
    (let ((output-file (output-file o c)))
      (dump-image output-file))))

(defclass compiled-file (file-component)
  ((type :initform #-(or ecl mkcl) (compile-file-type) #+(or ecl mkcl) "fasb")))

(defclass precompiled-system (system)
  ((bundle-pathname :initarg :fasl)))

(defclass prebuilt-system (system)
  ((bundle-pathname :initarg :static-library :initarg :lib
                    :accessor prebuilt-system-static-library)))

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
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
          (unless (typep instance 'program-op)
            (if (operation-monolithic-p instance) ".all-systems" #-ecl ".system"))))
  (when (typep instance 'monolithic-bundle-op)
    (destructuring-bind (&rest original-initargs
                         &key lisp-files prologue-code epilogue-code
                         &allow-other-keys)
        (operation-original-initargs instance)
      (setf (operation-original-initargs instance)
            (remove-plist-keys '(:lisp-files :epilogue-code :prologue-code) original-initargs)
            (monolithic-op-prologue-code instance) prologue-code
            (monolithic-op-epilogue-code instance) epilogue-code)
      #-ecl (assert (null (or lisp-files epilogue-code prologue-code)))
      #+ecl (setf (bundle-op-lisp-files instance) lisp-files)))
  (setf (bundle-op-build-args instance)
        (remove-plist-keys '(:type :monolithic :name-suffix)
                     (operation-original-initargs instance))))

(defmethod bundle-op-build-args :around ((o lib-op))
  (declare (ignorable o))
  (let ((args (call-next-method)))
    (remf args :ld-flags)
    args))

(defclass filtered-sequential-plan (sequential-plan)
  ((action-filter :initarg :action-filter :reader plan-action-filter)))

(defmethod action-valid-p ((plan filtered-sequential-plan) o c)
  (and (funcall (plan-action-filter plan) o c) (call-next-method)))

(defun* gather-actions (operation component &key other-systems (filter t))
  ;; This function creates a list of sub-actions performed
  ;; while building the targeted action.
  ;; This list may be restricted to sub-components of SYSTEM
  ;; if OTHER-SYSTEMS is NIL (default).
  (traverse operation component
            :plan-class 'filtered-sequential-plan
            :action-filter (ensure-function filter)
            :force (if other-systems :all t)
            :force-not (if other-systems nil :all)))

(defun* bundlable-file-p (pathname)
  (let ((type (pathname-type pathname)))
    (declare (ignorable type))
    (or #+ecl (or (equal type (compile-file-type :type :object))
                  (equal type (compile-file-type :type :static-library)))
        #+mkcl (equal type (compile-file-type :fasl-p nil))
        #+(or allegro clisp clozure cmu lispworks sbcl scl xcl) (equal type (compile-file-type)))))

(defun* operated-components (system &key (goal-operation 'load-op) (keep-operation goal-operation)
                                    (component-type t) (keep-component t) other-systems)
  (let ((goal-op (make-operation goal-operation)))
    (flet ((filter (o c)
             (declare (ignore o))
             (or (eq c system)
                 (typep c component-type))))
      (loop :for (o . c) :in (gather-actions goal-op system
                                             :other-systems other-systems
                                             :filter #'filter)
            :when (and (typep o keep-operation) (typep c keep-component))
              :collect c))))

(defgeneric* trivial-system-p (component))

(defun* user-system-p (s)
  (and (typep s 'system)
       (not (builtin-system-p s))
       (not (trivial-system-p s))))

(deftype user-system () '(and system (satisfies user-system-p)))

;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;

(defmethod component-depends-on ((o monolithic-lib-op) (c system))
  (declare (ignorable o))
  `((lib-op ,@(operated-components c :other-systems t :component-type 'system
                                     :goal-operation 'load-op
                                     :keep-operation 'load-op))))

(defmethod component-depends-on ((o monolithic-fasl-op) (c system))
  (declare (ignorable o))
  `((fasl-op ,@(operated-components c :other-systems t :component-type 'system
                                      :goal-operation 'load-fasl-op
                                      :keep-operation 'load-fasl-op))))

(defmethod component-depends-on ((o program-op) (c system))
  (declare (ignorable o))
  #+(or ecl mkcl) (component-depends-on (make-operation 'monolithic-lib-op) c)
  #-(or ecl mkcl) `((load-op ,c)))

(defmethod component-depends-on ((o binary-op) (c system))
  (declare (ignorable o))
  `((fasl-op ,c)
    (lib-op ,c)))

(defmethod component-depends-on ((o monolithic-binary-op) (c system))
  `((,(find-operation o 'monolithic-fasl-op) ,c)
    (,(find-operation o 'monolithic-lib-op) ,c)))

(defmethod component-depends-on ((o lib-op) (c system))
  (declare (ignorable o))
  `((compile-op ,@(operated-components c :other-systems nil :component-type '(not system)
                                         :goal-operation 'load-op
                                         :keep-operation 'load-op))))

(defmethod component-depends-on ((o fasl-op) (c system))
  (declare (ignorable o))
  #+ecl `((lib-op ,c))
  #-ecl
  (component-depends-on (find-operation o 'lib-op) c))

(defmethod component-depends-on ((o dll-op) c)
  (component-depends-on (find-operation o 'lib-op) c))

(defmethod component-depends-on ((o bundle-op) c)
  (declare (ignorable o c))
  nil)

(defmethod component-depends-on :around ((o bundle-op) (c component))
  (declare (ignorable o c))
  (if-let (op (and (eq (type-of o) 'bundle-op) (component-bundle-operation c)))
      `((,op ,c))
      (call-next-method)))

(defun* dependency-files (o c &key (test 'identity) (key 'output-files))
  (while-collecting (collect)
    (visit-dependencies
     () o c #'(lambda (sub-o sub-c)
                (loop :for f :in (funcall key sub-o sub-c)
                      :when (funcall test f) :do (collect f))))))

(defmethod input-files ((o bundle-op) (c system))
  (dependency-files o c :test 'bundlable-file-p :key 'output-files))

(defun* select-bundle-operation (type &optional monolithic)
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
  (let* ((operation-name (select-bundle-operation type monolithic))
         (move-here-path (if (and move-here
                                  (typep move-here '(or pathname string)))
                             (pathname move-here)
                             (merge-pathnames "./asdf-output/")))
         (operation (apply #'operate operation-name
                           system
                           (remove-plist-keys '(:monolithic :type :move-here) args)))
         (system (find-system system))
         (files (and system (output-files operation system))))
    (if (or move-here (and (null move-here-p)
                           (member operation-name '(:program :binary))))
        (loop :with dest-path = (resolve-symlinks* (ensure-directories-exist move-here-path))
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
  `((,o ,@(loop :for dep :in (component-sibling-dependencies c)
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
  (mark-operation-done (find-operation o 'load-op) c))

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

(defmethod perform ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod component-depends-on ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod component-depends-on ((o monolithic-lib-op) (c prebuilt-system))
  (declare (ignorable o))
  nil)


;;;
;;; PREBUILT SYSTEM CREATOR
;;;


(defmethod output-files ((o binary-op) (s system))
  (list (make-pathname :name (component-name s) :type "asd"
                       :defaults (component-pathname s))))

(defmethod perform ((o binary-op) (s system))
  (let* ((dependencies (component-depends-on o s))
         (fasl (first (apply #'output-files (first dependencies))))
         (library (first (apply #'output-files (second dependencies))))
         (asd (first (output-files o s)))
         (name (pathname-name asd))
         (name-keyword (intern (string name) (find-package :keyword))))
    (with-open-file (s asd :direction :output :if-exists :supersede
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
                     :lib ,(and library (file-namestring library)))
                s)))))

#-(or ecl mkcl)
(defmethod perform ((o fasl-op) (c system))
  (let* ((input-files (input-files o c))
         (fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test-not #'string=))
         (non-fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test #'string=))
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

(defmethod input-files ((o load-op) (s precompiled-system))
  (declare (ignorable o))
  (bundle-output-files (find-operation o 'fasl-op) s))

(defmethod component-depends-on ((o load-fasl-op) (s precompiled-system))
  (declare (ignorable o))
  `((load-op ,s) ,@(call-next-method)))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+ecl
(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (input-files o c))
         (output (output-files o c))
         (bundle (first output))
         (kind (bundle-type o))
         (init-name (c::compute-init-name bundle :kind kind)))
    (apply #'c::builder kind (first output)
           :init-name init-name
           :lisp-files (append object-files (bundle-op-lisp-files o))
           (append (bundle-op-build-args o)
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-prologue-code o))
                     `(:prologue-code ,(monolithic-op-prologue-code o)))
                   (when (typep o 'program-op)
                     `(:epilogue-code
                       (restore-image :entry-point ,(component-entry-point c))))
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-epilogue-code o))
                     `(:epilogue-code ,(monolithic-op-epilogue-code o)))))))

#+mkcl
(progn
  (defmethod perform ((o lib-op) (s system))
    (apply #'compiler::build-static-library (first output)
           :lisp-object-files (input-files o s) (bundle-op-build-args o)))

  (defmethod perform ((o fasl-op) (s system))
    (apply #'compiler::build-bundle (second output)
           :lisp-object-files (input-files o s) (bundle-op-build-args o)))

  (defun* bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
    (declare (ignore force verbose version))
    (apply #'operate 'binary-op system args)))

#+(or ecl mkcl)
(defun* register-pre-built-system (name)
  (register-system (make-instance 'system :name (coerce-name name) :source-file nil)))
