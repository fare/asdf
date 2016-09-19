;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(uiop/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate :asdf/defsystem)
  (:export
   #:bundle-op #:bundle-type #:program-system
   #:bundle-system #:bundle-pathname-type #:direct-dependency-files
   #:monolithic-op #:monolithic-bundle-op #:operation-monolithic-p
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:monolithic-lib-op
   #:dll-op #:monolithic-dll-op
   #:deliver-asd-op #:monolithic-deliver-asd-op
   #:program-op #:image-op #:compiled-file #:precompiled-system #:prebuilt-system
   #:user-system-p #:user-system #:trivial-system-p
   #:make-build
   #:build-args #:name-suffix #:prologue-code #:epilogue-code #:static-library))
(in-package :asdf/bundle)

(with-upgradability ()
  (defclass bundle-op (basic-compile-op)
    ;; NB: use of instance-allocated slots for operations is DEPRECATED
    ;; and only supported in a temporary fashion for backward compatibility.
    ;; Supported replacement: Define slots on program-system instead.
    ((build-args :initarg :args :initform nil :accessor extra-build-args)
     (name-suffix :initarg :name-suffix :initform nil)
     (bundle-type :initform :no-output-file :reader bundle-type)
     #+(or clasp ecl) (lisp-files :initform nil :accessor extra-object-files))
    (:documentation "base class for operations that bundle outputs from multiple components"))

  (defclass monolithic-op (operation) ()
    (:documentation "A MONOLITHIC operation operates on a system *and all of its
dependencies*.  So, for example, a monolithic concatenate operation will
concatenate together a system's components and all of its dependencies, but a
simple concatenate operation will concatenate only the components of the system
itself."))

  (defclass monolithic-bundle-op (monolithic-op bundle-op)
    ;; Old style way of specifying prologue and epilogue on ECL: in the monolithic operation.
    ;; DEPRECATED. Supported replacement: Define slots on program-system instead.
    ((prologue-code :initform nil :accessor prologue-code)
     (epilogue-code :initform nil :accessor epilogue-code))
    (:documentation "operations that are both monolithic-op and bundle-op"))

  (defclass program-system (system)
    ;; New style (ASDF3.1) way of specifying prologue and epilogue on ECL: in the system
    ((prologue-code :initform nil :initarg :prologue-code :reader prologue-code)
     (epilogue-code :initform nil :initarg :epilogue-code :reader epilogue-code)
     (no-uiop :initform nil :initarg :no-uiop :reader no-uiop)
     (prefix-lisp-object-files :initarg :prefix-lisp-object-files
                               :initform nil :accessor prefix-lisp-object-files)
     (postfix-lisp-object-files :initarg :postfix-lisp-object-files
                                :initform nil :accessor postfix-lisp-object-files)
     (extra-object-files :initarg :extra-object-files
                         :initform nil :accessor extra-object-files)
     (extra-build-args :initarg :extra-build-args
                       :initform nil :accessor extra-build-args)))

  (defmethod prologue-code ((x t)) nil)
  (defmethod epilogue-code ((x t)) nil)
  (defmethod no-uiop ((x t)) nil)
  (defmethod prefix-lisp-object-files ((x t)) nil)
  (defmethod postfix-lisp-object-files ((x t)) nil)
  (defmethod extra-object-files ((x t)) nil)
  (defmethod extra-build-args ((x t)) nil)

  (defclass link-op (bundle-op) ()
    (:documentation "Abstract operation for linking files together"))

  (defclass gather-op (bundle-op)
    ((gather-op :initform nil :allocation :class :reader gather-op)
     (gather-type :initform :no-output-file :allocation :class :reader gather-type))
    (:documentation "Abstract operation for gathering many input files from a system"))

  (defun operation-monolithic-p (op)
    (typep op 'monolithic-op))

  ;; Dependencies of a gather-op are the actions of the dependent operation
  ;; (from gather-op reader, defaulting to lib-op or compile-op depending
  ;; on the operation being monolithic or not) for all the (sorted) required components
  ;; for loading the system, recursing either to other systems or sub-components
  ;; depending on it being monolithic or not.
  (defmethod component-depends-on ((o gather-op) (s system))
    (let* ((mono (operation-monolithic-p o))
           (deps
             (required-components
              s :other-systems mono :component-type (if mono 'system '(not system))
                :goal-operation (find-operation o 'load-op)
                :keep-operation 'compile-op)))
      ;; NB: the explicit make-operation on ECL and MKCL
      ;; ensures that we drop the original-initargs and its magic flags when recursing.
      `((,(make-operation (or (gather-op o) (if mono 'lib-op 'compile-op))) ,@deps)
        ,@(call-next-method))))

  ;; Create a single fasl for the entire library
  (defclass basic-compile-bundle-op (bundle-op)
    ((gather-type :initform #-(or clasp ecl mkcl) :fasl #+(or clasp ecl mkcl) :object
                  :allocation :class)
     (bundle-type :initform :fasl :allocation :class))
    (:documentation "Base class for compiling into a bundle"))

  ;; Analog to prepare-op, for load-bundle-op and compile-bundle-op
  (defclass prepare-bundle-op (sideway-operation)
    ((sideway-operation
      :initform #+(or clasp ecl mkcl) 'load-bundle-op #-(or clasp ecl mkcl) 'load-op
      :allocation :class))
    (:documentation "Operation class for loading the bundles of a system's dependencies"))

  (defclass lib-op (link-op gather-op non-propagating-operation)
    ((gather-type :initform :object :allocation :class)
     (bundle-type :initform :lib :allocation :class))
    (:documentation "Compile the system and produce a linkable static library (.a/.lib)
for all the linkable object files associated with the system. Compare with DLL-OP.

On most implementations, these object files only include extensions to the runtime
written in C or another language with a compiler producing linkable object files.
On CLASP, ECL, MKCL, these object files _also_ include the contents of Lisp files
themselves. In any case, this operation will produce what you need to further build
a static runtime for your system, or a dynamic library to load in an existing runtime."))

  (defclass compile-bundle-op (basic-compile-bundle-op selfward-operation
                               #+(or clasp ecl mkcl) link-op #-(or clasp ecl) gather-op)
    ((selfward-operation :initform '(prepare-bundle-op #+(or clasp ecl) lib-op)
                         :allocation :class))
    (:documentation "This operator is an alternative to COMPILE-OP. Build a system
and all of its dependencies, but build only a single (\"monolithic\") FASL, instead
of one per source file, which may be more resource efficient.  That monolithic
FASL should be loaded with LOAD-BUNDLE-OP, rather than LOAD-OP."))

  (defclass load-bundle-op (basic-load-op selfward-operation)
    ((selfward-operation :initform '(prepare-bundle-op compile-bundle-op) :allocation :class))
    (:documentation "This operator is an alternative to LOAD-OP. Build a system
and all of its dependencies, using COMPILE-BUNDLE-OP. The difference with
respect to LOAD-OP is that it builds only a single FASL, which may be
faster and more resource efficient."))

  ;; NB: since the monolithic-op's can't be sideway-operation's,
  ;; if we wanted lib-op, dll-op, deliver-asd-op to be sideway-operation's,
  ;; we'd have to have the monolithic-op not inherit from the main op,
  ;; but instead inherit from a basic-FOO-op as with basic-compile-bundle-op above.

  (defclass dll-op (link-op gather-op non-propagating-operation)
    ((gather-type :initform :object :allocation :class)
     (bundle-type :initform :dll :allocation :class))
    (:documentation "Compile the system and produce a dynamic loadable library (.so/.dll)
for all the linkable object files associated with the system. Compare with LIB-OP."))

  (defclass deliver-asd-op (basic-compile-op selfward-operation)
    ((selfward-operation
      ;; TODO: implement link-op on all implementations, and make that
      ;; '(compile-bundle-op lib-op #-(or clasp ecl mkcl) dll-op)
      :initform '(compile-bundle-op #+(or clasp ecl mkcl) lib-op)
      :allocation :class))
    (:documentation "produce an asd file for delivering the system as a single fasl"))


  (defclass monolithic-deliver-asd-op (monolithic-bundle-op deliver-asd-op)
    ((selfward-operation
      ;; TODO: implement link-op on all implementations, and make that
      ;; '(monolithic-compile-bundle-op monolithic-lib-op #-(or clasp ecl mkcl) monolithic-dll-op)
      :initform '(monolithic-compile-bundle-op #+(or clasp ecl mkcl) monolithic-lib-op)
      :allocation :class))
    (:documentation "produce fasl and asd files for combined system and dependencies."))

  (defclass monolithic-compile-bundle-op
      (monolithic-bundle-op basic-compile-bundle-op
       #+(or clasp ecl mkcl) link-op gather-op non-propagating-operation)
    ((gather-op :initform #-(or clasp ecl mkcl) 'compile-bundle-op #+(or clasp ecl mkcl) 'lib-op
                :allocation :class)
     (gather-type :initform #-(or clasp ecl mkcl) :fasl #+(or clasp ecl mkcl) :static-library
                  :allocation :class))
    (:documentation "Create a single fasl for the system and its dependencies."))

  (defclass monolithic-load-bundle-op (monolithic-bundle-op load-bundle-op)
    ((selfward-operation :initform 'monolithic-compile-bundle-op :allocation :class))
    (:documentation "Load a single fasl for the system and its dependencies."))

  (defclass monolithic-lib-op (monolithic-bundle-op lib-op non-propagating-operation)
    ((gather-type :initform :static-library :allocation :class))
    (:documentation "Compile the system and produce a linkable static library (.a/.lib)
for all the linkable object files associated with the system or its dependencies. See LIB-OP."))

  (defclass monolithic-dll-op (monolithic-bundle-op dll-op non-propagating-operation)
    ((gather-type :initform :static-library :allocation :class))
    (:documentation "Compile the system and produce a dynamic loadable library (.so/.dll)
for all the linkable object files associated with the system or its dependencies. See LIB-OP"))

  (defclass image-op (monolithic-bundle-op selfward-operation
                      #+(or clasp ecl mkcl) link-op #+(or clasp ecl mkcl) gather-op)
    ((bundle-type :initform :image)
     #+(or clasp ecl mkcl) (gather-type :initform :static-library :allocation :class)
     (selfward-operation :initform '(#-(or clasp ecl mkcl) load-op) :allocation :class))
    (:documentation "create an image file from the system and its dependencies"))

  (defclass program-op (image-op)
    ((bundle-type :initform :program))
    (:documentation "create an executable file from the system and its dependencies"))

  ;; From the ASDF-internal bundle-type identifier, get a filesystem-usable pathname type.
  (defun bundle-pathname-type (bundle-type)
    (etypecase bundle-type
      ((or null string) ;; pass through nil or string literal
       bundle-type)
      ((eql :no-output-file) ;; marker for a bundle-type that has NO output file
       (error "No output file, therefore no pathname type"))
      ((eql :fasl) ;; the type of a fasl
       #-(or clasp ecl mkcl) (compile-file-type) ; on image-based platforms, used as input and output
       #+(or clasp ecl mkcl) "fasb") ; on C-linking platforms, only used as output for system bundles
      ((member :image)
       #+allegro "dxl"
       #+(and clisp os-windows) "exe"
       #-(or allegro (and clisp os-windows)) "image")
      ;; NB: on CLASP and ECL these implementations, we better agree with
      ;; (compile-file-type :type bundle-type))
      ((eql :object) ;; the type of a linkable object file
       (os-cond ((os-unix-p) "o")
                ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "o" "obj"))))
      ((member :lib :static-library) ;; the type of a linkable library
       (os-cond ((os-unix-p) "a")
                ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "a" "lib"))))
      ((member :dll :shared-library) ;; the type of a shared library
       (os-cond ((os-macosx-p) "dylib") ((os-unix-p) "so") ((os-windows-p) "dll")))
      ((eql :program) ;; the type of an executable program
       (os-cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

  ;; Compute the output-files for a given bundle action
  (defun bundle-output-files (o c)
    (let ((bundle-type (bundle-type o)))
      (unless (or (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
                  (and (null (input-files o c)) (not (member bundle-type '(:image :program)))))
        (let ((name (or (component-build-pathname c)
                        (format nil "~A~@[~A~]" (component-name c) (slot-value o 'name-suffix))))
              (type (bundle-pathname-type bundle-type)))
          (values (list (subpathname (component-pathname c) name :type type))
                  (eq (class-of o) (coerce-class (component-build-operation c)
                                                 :package :asdf/interface
                                                 :super 'operation
                                                 :error nil)))))))

  (defmethod output-files ((o bundle-op) (c system))
    (bundle-output-files o c))

  #-(or clasp ecl mkcl)
  (progn
    (defmethod perform ((o image-op) (c system))
      (dump-image (output-file o c) :executable (typep o 'program-op)))
    (defmethod perform :before ((o program-op) (c system))
      (setf *image-entry-point* (ensure-function (component-entry-point c)))))

  (defclass compiled-file (file-component)
    ((type :initform #-(or clasp ecl mkcl) (compile-file-type) #+(or clasp ecl mkcl) "fasb"))
    (:documentation "Class for a file that is already compiled,
e.g. as part of the implementation, of an outer build system that calls into ASDF,
or of opaque libraries shipped along the source code."))

  (defclass precompiled-system (system)
    ((build-pathname :initarg :fasl))
    (:documentation "Class For a system that is delivered as a precompiled fasl"))

  (defclass prebuilt-system (system)
    ((build-pathname :initarg :static-library :initarg :lib
                     :accessor prebuilt-system-static-library))
    (:documentation "Class for a system that comes precompiled with a static library file")))


;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;
(with-upgradability ()
  (defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                         &key (name-suffix nil name-suffix-p)
                                         &allow-other-keys)
    (declare (ignore initargs name-suffix))
    ;; TODO: make that class slots or methods, not instance slots
    (unless name-suffix-p
      (setf (slot-value instance 'name-suffix)
            (unless (typep instance 'program-op)
              ;; "." is no good separator for Logical Pathnames, so we use "--"
              (if (operation-monolithic-p instance) "--all-systems" #-(or clasp ecl mkcl) "--system"))))
    (when (typep instance 'monolithic-bundle-op)
      (destructuring-bind (&key lisp-files prologue-code epilogue-code
                           &allow-other-keys)
          (operation-original-initargs instance)
        (setf (prologue-code instance) prologue-code
              (epilogue-code instance) epilogue-code)
        #-(or clasp ecl) (assert (null (or lisp-files #-mkcl epilogue-code #-mkcl prologue-code)))
        #+(or clasp ecl) (setf (extra-object-files instance) lisp-files)))
    (setf (extra-build-args instance)
          (remove-plist-keys
           '(:type :monolithic :name-suffix :epilogue-code :prologue-code :lisp-files
             :force :force-not :plan-class) ;; TODO: refactor so we don't mix plan and operation arguments
           (operation-original-initargs instance))))

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
  (defun direct-dependency-files (o c &key (test 'identity) (key 'output-files) &allow-other-keys)
    ;; This function selects output files from direct dependencies;
    ;; your component-depends-on method must gather the correct dependencies in the correct order.
    (while-collecting (collect)
      (map-direct-dependencies
       t o c #'(lambda (sub-o sub-c)
                 (loop :for f :in (funcall key sub-o sub-c)
                       :when (funcall test f) :do (collect f))))))

  (defun pathname-type-equal-function (type)
    #'(lambda (p) (equalp (pathname-type p) type)))

  (defmethod input-files ((o gather-op) (c system))
    (unless (eq (bundle-type o) :no-output-file)
      (direct-dependency-files
       o c :key 'output-files
           :test (pathname-type-equal-function (bundle-pathname-type (gather-type o))))))

  ;; Find the operation that produces a given bundle-type
  (defun select-bundle-operation (type &optional monolithic)
    (ecase type
      ((:dll :shared-library)
       (if monolithic 'monolithic-dll-op 'dll-op))
      ((:lib :static-library)
       (if monolithic 'monolithic-lib-op 'lib-op))
      ((:fasl)
       (if monolithic 'monolithic-compile-bundle-op 'compile-bundle-op))
      ((:image)
       'image-op)
      ((:program)
       'program-op)))

  ;; DEPRECATED. This is originally from asdf-ecl.lisp.
  ;; It must die, and so must any use of initargs in operation,
  ;; unless keys to the asdf-cache are substantially modified to accommodate for them.
  ;; Coordinate with the ECL maintainers to get them to stop using it.
  ;; SUPPORTED REPLACEMENT: Use program-op and program-system
  (defun make-build (system &rest args &key (monolithic nil) (type :fasl)
                             (move-here nil move-here-p)
                             &allow-other-keys)
    (let* ((operation-name (select-bundle-operation type monolithic))
           (move-here-path (if (and move-here
                                    (typep move-here '(or pathname string)))
                               (ensure-pathname move-here :namestring :lisp :ensure-directory t)
                               (system-relative-pathname system "asdf-output/")))
           (operation (apply 'operate operation-name
                             system
                             (remove-plist-keys '(:monolithic :type :move-here) args)))
           (system (find-system system))
           (files (and system (output-files operation system))))
      (if (or move-here (and (null move-here-p)
                             (member operation-name '(:program :image))))
          (loop :with dest-path = (resolve-symlinks* (ensure-directories-exist move-here-path))
                :for f :in files
                :for new-f = (make-pathname :name (pathname-name f)
                                            :type (pathname-type f)
                                            :defaults dest-path)
                :do (handler-case (rename-file-overwriting-target f new-f)
                      (file-error (c)
                        (declare (ignore c))
                        (copy-file f new-f)
                        (delete-file-if-exists f)))
                :collect new-f)
          files)))

  ;; DEPRECATED. Apparently, some users of ECL, MKCL and ABCL may still be using it;
  ;; but at the very least, this function should be renamed, and/or
  ;; some way of specifying the output directory should be provided.
  ;; As is, it is not such a useful interface.
  (defun bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
    (declare (ignore force verbose version))
    (apply 'operate 'deliver-asd-op system args)))

;;;
;;; LOAD-BUNDLE-OP
;;;
;;; This is like ASDF's LOAD-OP, but using bundle fasl files.
;;;
(with-upgradability ()
  (defmethod component-depends-on ((o load-bundle-op) (c system))
    `((,o ,@(component-sideway-dependencies c))
      (,(if (user-system-p c) 'compile-bundle-op 'load-op) ,c)
      ,@(call-next-method)))

  (defmethod input-files ((o load-bundle-op) (c system))
    (when (user-system-p c)
      (output-files (find-operation o 'compile-bundle-op) c)))

  (defmethod perform ((o load-bundle-op) (c system))
    (when (input-files o c)
      (perform-lisp-load-fasl o c)))

  (defmethod mark-operation-done :after ((o load-bundle-op) (c system))
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
  (defmethod perform ((o operation) (c compiled-file))
    nil))

;;;
;;; Pre-built systems
;;;
(with-upgradability ()
  (defmethod trivial-system-p ((s prebuilt-system))
    t)

  (defmethod perform ((o link-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o basic-compile-bundle-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o lib-op) (c prebuilt-system))
    nil)

  (defmethod perform ((o dll-op) (c prebuilt-system))
    nil)

  (defmethod component-depends-on ((o gather-op) (c prebuilt-system))
    nil)

  (defmethod output-files ((o lib-op) (c prebuilt-system))
    (values (list (prebuilt-system-static-library c)) t)))


;;;
;;; PREBUILT SYSTEM CREATOR
;;;
(with-upgradability ()
  (defmethod output-files ((o deliver-asd-op) (s system))
    (list (make-pathname :name (component-name s) :type "asd"
                         :defaults (component-pathname s))))

  (defmethod perform ((o deliver-asd-op) (s system))
    (let* ((inputs (input-files o s))
           (fasl (first inputs))
           (library (second inputs))
           (asd (first (output-files o s)))
           (name (if (and fasl asd) (pathname-name asd) (return-from perform)))
           (version (component-version s))
           (dependencies
             (if (operation-monolithic-p o)
                 (remove-if-not 'builtin-system-p
                                (required-components s :component-type 'system
                                                       :keep-operation 'load-op))
                 (while-collecting (x) ;; resolve the sideway-dependencies of s
                   (map-direct-dependencies
                    t 'load-op s
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
                     :version ,version
                     :depends-on ,depends-on
                     :components ((:compiled-file ,(pathname-name fasl)))
                     ,@(when library `(:lib ,(file-namestring library))))
                  s)
          (terpri s)))))

  #-(or clasp ecl mkcl)
  (defmethod perform ((o basic-compile-bundle-op) (c system))
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
        (when (or (prologue-code o) (epilogue-code o)
                  (prologue-code c) (epilogue-code c))
          (error "prologue-code and epilogue-code are not supported on ~A"
                 (implementation-type)))
        (with-staging-pathname (output-file)
          (combine-fasls fasl-files output-file)))))

  (defmethod input-files ((o load-op) (s precompiled-system))
    (bundle-output-files (find-operation o 'compile-bundle-op) s))

  (defmethod perform ((o load-op) (s precompiled-system))
    (perform-lisp-load-fasl o s))

  (defmethod component-depends-on ((o load-bundle-op) (s precompiled-system))
    #+xcl (declare (ignorable o))
    `((load-op ,s) ,@(call-next-method))))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+(or clasp ecl mkcl)
(with-upgradability ()

  #+ecl ;; doesn't work on clasp or mkcl (yet?).
  (unless (use-ecl-byte-compiler-p)
    (setf *load-system-operation* 'load-bundle-op))

  (defun system-module-pathname (module)
    (let ((name (coerce-name module)))
      (some
       'file-exists-p
       (list
        #+clasp (compile-file-pathname (make-pathname :name name :defaults "sys:") :output-type :object)
        #+ecl (compile-file-pathname (make-pathname :name name :defaults "sys:") :type :lib)
        #+ecl (compile-file-pathname (make-pathname :name name :defaults "sys:") :type :object)
        #+mkcl (make-pathname :name name :type (bundle-pathname-type :lib) :defaults #p"sys:")
        #+mkcl (make-pathname :name name :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;")))))

  (defun make-prebuilt-system (name &optional (pathname (system-module-pathname name)))
    "Creates a prebuilt-system if PATHNAME isn't NIL."
    (when pathname
      (make-instance 'prebuilt-system
                     :name (coerce-name name)
                     :static-library (resolve-symlinks* pathname))))

  (defmethod component-depends-on :around ((o image-op) (c system))
    (destructuring-bind ((lib-op . deps)) (call-next-method)
      (labels ((has-it-p (x) (find x deps :test 'equal :key 'coerce-name))
               (ensure-linkable-system (x)
		 (unless (has-it-p x)
                   (or (if-let (s (find-system x))
                         (and (system-source-directory x)
                              (list s)))
                       (if-let (p (system-module-pathname x))
                         (list (make-prebuilt-system x p)))))))
        `((,lib-op
           ,@(unless (no-uiop c)
               (append (ensure-linkable-system "cmp")
                       (or (ensure-linkable-system "uiop")
                           (ensure-linkable-system "asdf"))))
           ,@deps)))))

  (defmethod perform ((o link-op) (c system))
    (let* ((object-files (input-files o c))
           (output (output-files o c))
           (bundle (first output))
           (programp (typep o 'program-op))
           (kind (bundle-type o)))
      (when output
        (apply 'create-image
               bundle (append
                       (when programp (prefix-lisp-object-files c))
                       object-files
                       (when programp (postfix-lisp-object-files c)))
               :kind kind
               :prologue-code (or (prologue-code o) (when programp (prologue-code c)))
               :epilogue-code (or (epilogue-code o) (when programp (epilogue-code c)))
               :build-args (or (extra-build-args o) (when programp (extra-build-args c)))
               :extra-object-files (or (extra-object-files o) (when programp (extra-object-files c)))
               :no-uiop (no-uiop c)
               (when programp `(:entry-point ,(component-entry-point c))))))))
