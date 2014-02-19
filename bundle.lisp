;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(uiop/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate)
  (:export
   #:bundle-op #:bundle-op-build-args #:bundle-type
   #:bundle-system #:bundle-pathname-type #:bundlable-file-p #:direct-dependency-files
   #:monolithic-op #:monolithic-bundle-op #:operation-monolithic-p
   #:basic-fasl-op #:prepare-fasl-op #:fasl-op #:load-fasl-op #:monolithic-fasl-op
   #:lib-op #:monolithic-lib-op
   #:dll-op #:monolithic-dll-op
   #:binary-op #:monolithic-binary-op
   #:program-op #:compiled-file #:precompiled-system #:prebuilt-system
   #:user-system-p #:user-system #:trivial-system-p
   #+ecl #:make-build
   #:register-pre-built-system
   #:build-args #:name-suffix #:prologue-code #:epilogue-code #:static-library))
(in-package :asdf/bundle)

(with-upgradability ()
  (defclass bundle-op (operation)
    ((build-args :initarg :args :initform nil :accessor bundle-op-build-args)
     (name-suffix :initarg :name-suffix :initform nil)
     (bundle-type :initform :no-output-file :reader bundle-type)
     #+ecl (lisp-files :initform nil :accessor bundle-op-lisp-files)
     #+mkcl (do-fasb :initarg :do-fasb :initform t :reader bundle-op-do-fasb-p)
     #+mkcl (do-static-library :initarg :do-static-library :initform t :reader bundle-op-do-static-library-p)))

  (defclass bundle-compile-op (bundle-op basic-compile-op)
    ()
    (:documentation "Abstract operation for ways to bundle the outputs of compiling *Lisp* files"))

  ;; create a single fasl for the entire library
  (defclass basic-fasl-op (bundle-compile-op)
    ((bundle-type :initform :fasl)))
  (defclass prepare-fasl-op (sideway-operation)
    ((sideway-operation :initform 'load-fasl-op :allocation :class)))
  (defclass fasl-op (basic-fasl-op selfward-operation)
    ((selfward-operation :initform '(prepare-fasl-op #+ecl lib-op) :allocation :class)))
  (defclass load-fasl-op (basic-load-op selfward-operation)
    ((selfward-operation :initform '(prepare-op fasl-op) :allocation :class)))

  ;; NB: since the monolithic-op's can't be sideway-operation's,
  ;; if we wanted lib-op, dll-op, binary-op to be sideway-operation's,
  ;; we'd have to have the monolithic-op not inherit from the main op,
  ;; but instead inherit from a basic-FOO-op as with basic-fasl-op above.

  (defclass no-ld-flags-op (operation) ())

  (defclass lib-op (bundle-compile-op no-ld-flags-op non-propagating-operation)
    ((bundle-type :initform #+(or ecl mkcl) :lib #-(or ecl mkcl) :no-output-file))
    (:documentation #+(or ecl mkcl) "compile the system and produce linkable (.a) library for it."
     #-(or ecl mkcl) "just compile the system"))

  (defclass dll-op (bundle-compile-op no-ld-flags-op non-propagating-operation)
    ((bundle-type :initform :dll))
    (:documentation "compile the system and produce dynamic (.so/.dll) library for it."))

  (defclass binary-op (basic-compile-op selfward-operation)
    ((selfward-operation :initform '(fasl-op lib-op) :allocation :class))
    (:documentation "produce fasl and asd files for the system"))

  (defclass monolithic-op (operation) ()
    (:documentation "A MONOLITHIC operation operates on a system *and all of its
dependencies*.  So, for example, a monolithic concatenate operation will
concatenate together a system's components and all of its dependencies, but a
simple concatenate operation will concatenate only the components of the system
itself.")) ;; operation on a system and its dependencies

  (defclass monolithic-bundle-op (monolithic-op bundle-op)
    ((prologue-code :accessor monolithic-op-prologue-code)
     (epilogue-code :accessor monolithic-op-epilogue-code)))

  (defclass monolithic-bundle-compile-op (monolithic-bundle-op bundle-compile-op)
    ()
    (:documentation "Abstract operation for ways to bundle the outputs of compiling
*Lisp* files over a system, and all of its dependencies."))

  (defclass monolithic-binary-op (monolithic-op binary-op)
    ((selfward-operation :initform '(monolithic-fasl-op monolithic-lib-op) :allocation :class))
    (:documentation "produce fasl and asd files for combined system and dependencies."))

  (defclass monolithic-fasl-op (monolithic-bundle-compile-op basic-fasl-op non-propagating-operation) ()
    (:documentation "Create a single fasl for the system and its dependencies."))

  (defclass monolithic-lib-op (monolithic-bundle-compile-op non-propagating-operation no-ld-flags-op)
    ((bundle-type :initform #+(or ecl mkcl) :lib #-(or ecl mkcl) :no-output-file))
    (:documentation #+(or ecl mkcl) "Create a single linkable library for the system and its dependencies."
     #-(or ecl mkcl) "Compile a system and its dependencies."))

  (defclass monolithic-dll-op (monolithic-bundle-compile-op non-propagating-operation no-ld-flags-op)
    ((bundle-type :initform :dll))
    (:documentation "Create a single dynamic (.so/.dll) library for the system and its dependencies."))

  ;; Fare reports that the PROGRAM-OP doesn't need any propagation on MKCL or
  ;; ECL because the necessary dependency wrangling is done by other, earlier
  ;; operations. [2014/01/20:rpg]
  (defclass program-op #+(or mkcl ecl) (monolithic-bundle-compile-op non-propagating-operation)
            #-(or mkcl ecl) (monolithic-bundle-op selfward-operation)
    ((bundle-type :initform :program)
     #-(or mkcl ecl) (selfward-operation :initform 'load-op))
    (:documentation "create an executable file from the system and its dependencies"))

  (defun bundle-pathname-type (bundle-type)
    (etypecase bundle-type
      ((eql :no-output-file) nil) ;; should we error out instead?
      ((or null string) bundle-type)
      ((eql :fasl) #-(or ecl mkcl) (compile-file-type) #+(or ecl mkcl) "fasb")
      #+ecl
      ((member :binary :dll :lib :shared-library :static-library :program :object :program)
       (compile-file-type :type bundle-type))
      ((eql :binary) "image")
      ((eql :dll) (cond ((os-macosx-p) "dylib") ((os-unix-p) "so") ((os-windows-p) "dll")))
      ((member :lib :static-library) (cond ((os-unix-p) "a") ((os-windows-p) "lib")))
      ((eql :program) (cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

  (defun bundle-output-files (o c)
    (when (input-files o c)
      (let ((bundle-type (bundle-type o)))
        (unless (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
          (let ((name (or (component-build-pathname c)
                          (format nil "~A~@[~A~]" (component-name c) (slot-value o 'name-suffix))))
                (type (bundle-pathname-type bundle-type)))
            (values (list (subpathname (component-pathname c) name :type type))
                    (eq (type-of o) (component-build-operation c))))))))

  (defmethod output-files ((o bundle-op) (c system))
    (bundle-output-files o c))

  #-(or ecl mkcl)
  (defmethod perform ((o program-op) (c system))
    (let ((output-file (output-file o c)))
      (setf *image-entry-point* (ensure-function (component-entry-point c)))
      (dump-image output-file :executable t)))

  (defclass compiled-file (file-component)
    ((type :initform #-(or ecl mkcl) (compile-file-type) #+(or ecl mkcl) "fasb")))

  (defclass precompiled-system (system)
    ((build-pathname :initarg :fasl)))

  (defclass prebuilt-system (system)
    ((build-pathname :initarg :static-library :initarg :lib
                     :accessor prebuilt-system-static-library))))


;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;
(with-upgradability ()
  (defun operation-monolithic-p (op)
    (typep op 'monolithic-op))

  (defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                         &key (name-suffix nil name-suffix-p)
                                         &allow-other-keys)
    (declare (ignore initargs name-suffix))
    (unless name-suffix-p
      (setf (slot-value instance 'name-suffix)
            (unless (typep instance 'program-op)
              (if (operation-monolithic-p instance) "--all-systems" #-ecl "--system")))) ; . no good for Logical Pathnames
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

  (defmethod bundle-op-build-args :around ((o no-ld-flags-op))
    (let ((args (call-next-method)))
      (remf args :ld-flags)
      args))

  (defun bundlable-file-p (pathname)
    (let ((type (pathname-type pathname)))
      (declare (ignorable type))
      (or #+ecl (or (equalp type (compile-file-type :type :object))
                    (equalp type (compile-file-type :type :static-library)))
          #+mkcl (equalp type (compile-file-type :fasl-p nil))
          #+(or abcl allegro clisp clozure cmu lispworks sbcl scl xcl) (equalp type (compile-file-type)))))

  (defgeneric* (trivial-system-p) (component))

  (defun user-system-p (s)
    (and (typep s 'system)
         (not (builtin-system-p s))
         (not (trivial-system-p s)))))

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype user-system () '(and system (satisfies user-system-p))))

;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
(with-upgradability ()
  (defmethod component-depends-on ((o bundle-compile-op) (c system))
    `(,(if (operation-monolithic-p o)
           `(#-(or ecl mkcl) fasl-op #+(or ecl mkcl) lib-op
               ,@(required-components c :other-systems t :component-type 'system
                                        :goal-operation (find-operation o 'load-op)
                                        :keep-operation 'compile-op))
           `(compile-op
             ,@(required-components c :other-systems nil :component-type '(not system)
                                      :goal-operation (find-operation o 'load-op)
                                      :keep-operation 'compile-op)))
      ,@(call-next-method)))

  (defmethod component-depends-on :around ((o bundle-op) (c component))
    (if-let (op (and (eq (type-of o) 'bundle-op) (component-build-operation c)))
      `((,op ,c))
      (call-next-method)))

  (defun direct-dependency-files (o c &key (test 'identity) (key 'output-files) &allow-other-keys)
    ;; This file selects output files from direct dependencies;
    ;; your component-depends-on method better gathered the correct dependencies in the correct order.
    (while-collecting (collect)
      (map-direct-dependencies
       o c #'(lambda (sub-o sub-c)
               (loop :for f :in (funcall key sub-o sub-c)
                     :when (funcall test f) :do (collect f))))))

  (defmethod input-files ((o bundle-compile-op) (c system))
    (unless (eq (bundle-type o) :no-output-file)
      (direct-dependency-files o c :test 'bundlable-file-p :key 'output-files)))

  (defun select-bundle-operation (type &optional monolithic)
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

  (defun make-build (system &rest args &key (monolithic nil) (type :fasl)
                             (move-here nil move-here-p)
                             &allow-other-keys)
    (let* ((operation-name (select-bundle-operation type monolithic))
           (move-here-path (if (and move-here
                                    (typep move-here '(or pathname string)))
                               (ensure-pathname move-here :namestring :lisp :ensure-directory t)
                               (system-relative-pathname system "asdf-output/")))
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
          files))))

;;;
;;; LOAD-FASL-OP
;;;
;;; This is like ASDF's LOAD-OP, but using monolithic fasl files.
;;;
(with-upgradability ()
  (defmethod component-depends-on ((o load-fasl-op) (c system))
    `((,o ,@(loop :for dep :in (component-sideway-dependencies c)
                  :collect (resolve-dependency-spec c dep)))
      (,(if (user-system-p c) 'fasl-op 'load-op) ,c)
      ,@(call-next-method)))

  (defmethod input-files ((o load-fasl-op) (c system))
    (when (user-system-p c)
      (output-files (find-operation o 'fasl-op) c)))

  (defmethod perform ((o load-fasl-op) (c system))
    (when (input-files o c)
      (perform-lisp-load-fasl o c)))

  (defmethod mark-operation-done :after ((o load-fasl-op) (c system))
    (mark-operation-done (find-operation o 'load-op) c)))

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF systems in precompiled form.
;;; Only useful when the dependencies have also been precompiled.
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s system))
    (every #'(lambda (c) (typep c 'compiled-file)) (component-children s)))

  (defmethod input-files ((o operation) (c compiled-file))
    (list (component-pathname c)))
  (defmethod perform ((o load-op) (c compiled-file))
    (perform-lisp-load-fasl o c))
  (defmethod perform ((o load-source-op) (c compiled-file))
    (perform (find-operation o 'load-op) c))
  (defmethod perform ((o load-fasl-op) (c compiled-file))
    (perform (find-operation o 'load-op) c))
  (defmethod perform ((o operation) (c compiled-file))
    nil))

;;;
;;; Pre-built systems
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s prebuilt-system))
    t)

  (defmethod perform ((o lib-op) (c prebuilt-system))
    nil)

  (defmethod component-depends-on ((o lib-op) (c prebuilt-system))
    nil)

  (defmethod component-depends-on ((o monolithic-lib-op) (c prebuilt-system))
    nil))


;;;
;;; PREBUILT SYSTEM CREATOR
;;;
(with-upgradability ()
  (defmethod output-files ((o binary-op) (s system))
    (list (make-pathname :name (component-name s) :type "asd"
                         :defaults (component-pathname s))))

  (defmethod perform ((o binary-op) (s system))
    (let* ((inputs (input-files o s))
           (fasl (first inputs))
           (library (second inputs))
           (asd (first (output-files o s)))
           (name (if (and fasl asd) (pathname-name asd) (return-from perform)))
           (dependencies
             (if (operation-monolithic-p o)
                 (remove-if-not 'builtin-system-p
                                (required-components s :component-type 'system
                                                       :keep-operation 'load-op))
                 (while-collecting (x) ;; resolve the sideway-dependencies of s
                   (map-direct-dependencies
                    'load-op s
                    #'(lambda (o c)
                        (when (and (typep o 'load-op) (typep c 'system))
                          (x c)))))))
           (depends-on (mapcar 'coerce-name dependencies)))
      (when (pathname-equal asd (system-source-file s))
        (cerror "overwrite the asd file"
                "~/asdf-action:format-action/ is going to overwrite the system definition file ~S which is probably not what you want; you probably need to tweak your output translations."
                (cons o s) asd))
      (with-open-file (s asd :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
        (format s ";;; Prebuilt~:[~; monolithic~] ASDF definition for system ~A~%"
                (operation-monolithic-p o) name)
        (format s ";;; Built for ~A ~A on a ~A/~A ~A~%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                (software-type)
                (machine-type)
                (software-version))
        (let ((*package* (find-package :asdf-user)))
          (pprint `(defsystem ,name
                     :class prebuilt-system
                     :depends-on ,depends-on
                     :components ((:compiled-file ,(pathname-name fasl)))
                     ,@(when library `(:lib ,(file-namestring library))))
                  s)
          (terpri s)))))

  #-(or ecl mkcl)
  (defmethod perform ((o bundle-compile-op) (c system))
    (let* ((input-files (input-files o c))
           (fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test-not #'equalp))
           (non-fasl-files (remove (compile-file-type) input-files :key #'pathname-type :test #'equalp))
           (output-files (output-files o c))
           (output-file (first output-files)))
      (assert (eq (not input-files) (not output-files)))
      (when input-files
        (when non-fasl-files
          (error "On ~A, asdf/bundle can only bundle FASL files, but these were also produced: ~S"
                 (implementation-type) non-fasl-files))
        (when (and (typep o 'monolithic-bundle-op)
                   (or (monolithic-op-prologue-code o) (monolithic-op-epilogue-code o)))
          (error "prologue-code and epilogue-code are not supported on ~A"
                 (implementation-type)))
        (with-staging-pathname (output-file)
          (combine-fasls fasl-files output-file)))))

  (defmethod input-files ((o load-op) (s precompiled-system))
    (bundle-output-files (find-operation o 'fasl-op) s))

  (defmethod perform ((o load-op) (s precompiled-system))
    (perform-lisp-load-fasl o s))

  (defmethod component-depends-on ((o load-fasl-op) (s precompiled-system))
    `((load-op ,s) ,@(call-next-method))))

  #| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+(or ecl mkcl)
(with-upgradability ()
  (defun uiop-library-file ()
    (or (and (find-system :uiop nil)
             (system-source-directory :uiop)
             (progn
               (operate 'lib-op :uiop)
               (output-file 'lib-op :uiop)))
        (resolve-symlinks* (c::compile-file-pathname "sys:asdf" :type :lib))))
  (defmethod input-files :around ((o program-op) (c system))
    (let ((files (call-next-method))
          (plan (traverse-sub-actions o c :plan-class 'sequential-plan)))
      (unless (or (and (find-system :uiop nil)
                       (system-source-directory :uiop)
                       (plan-operates-on-p plan '("uiop")))
                  (and (system-source-directory :asdf)
                       (plan-operates-on-p plan '("asdf"))))
        (pushnew (uiop-library-file) files :test 'pathname-equal))
      files))

  (defun register-pre-built-system (name)
    (register-system (make-instance 'system :name (coerce-name name) :source-file nil))))

#+ecl
(with-upgradability ()
  (defmethod perform ((o bundle-compile-op) (c system))
    (let* ((object-files (input-files o c))
           (output (output-files o c))
           (bundle (first output))
           (kind (bundle-type o)))
      (when output
        (create-image
         bundle (append object-files (bundle-op-lisp-files o))
         :kind kind
         :entry-point (component-entry-point c)
         :prologue-code
         (when (typep o 'monolithic-bundle-op)
           (monolithic-op-prologue-code o))
         :epilogue-code
         (when (typep o 'monolithic-bundle-op)
           (monolithic-op-epilogue-code o))
         :build-args (bundle-op-build-args o))))))

#+mkcl
(with-upgradability ()
  (defmethod perform ((o lib-op) (s system))
    (apply #'compiler::build-static-library (output-file o c)
           :lisp-object-files (input-files o s) (bundle-op-build-args o)))

  (defmethod perform ((o basic-fasl-op) (s system))
    (apply #'compiler::build-bundle (output-file o c) ;; second???
           :lisp-object-files (input-files o s) (bundle-op-build-args o)))

  (defun bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
    (declare (ignore force verbose version))
    (apply #'operate 'binary-op system args)))

#+(and (not asdf-use-unsafe-mac-bundle-op)
       (or (and ecl darwin)
           (and abcl darwin (not abcl-bundle-op-supported))))
(defmethod perform :before ((o basic-fasl-op) (c component))
  (unless (featurep :asdf-use-unsafe-mac-bundle-op)
    (cerror "Continue after modifying *FEATURES*."
            "BASIC-FASL-OP bundle operations are not supported on Mac OS X for this lisp.~%~T~
To continue, push :asdf-use-unsafe-mac-bundle-op onto *FEATURES*.~%~T~
Please report to ASDF-DEVEL if this works for you.")))
