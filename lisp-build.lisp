;;;; -------------------------------------------------------------------------
;;;; Support to build (compile and load) Lisp files

(asdf/package:define-package :asdf/lisp-build
  (:recycle :asdf/interface :asdf :asdf/lisp-build)
  (:use :common-lisp :asdf/package :asdf/compatibility :asdf/utility
        :asdf/pathname :asdf/stream :asdf/os :asdf/image)
  (:export
   ;; Variables
   #:*compile-file-warnings-behaviour* #:*compile-file-failure-behaviour*
   #:*output-translation-function*
   #:*optimization-settings* #:*previous-optimization-settings*
   #:*uninteresting-compiler-conditions* #:*uninteresting-loader-conditions*
   #:*deferred-warnings*
   ;; Functions & Macros
   #:get-optimization-settings #:proclaim-optimization-settings
   #:call-with-muffled-compiler-conditions #:with-muffled-compiler-conditions
   #:call-with-muffled-loader-conditions #:with-muffled-loader-conditions
   #:call-with-asdf-compilation-unit #:with-asdf-compilation-unit
   #:current-lisp-file-pathname #:lispize-pathname #:compile-file-type #:call-around-hook
   #:compile-file* #:compile-file-pathname*
   #:load* #:load-from-string #:combine-fasls)
  (:intern #:defaults #:failure-p #:warnings-p))
(in-package :asdf/lisp-build)

(defvar *compile-file-warnings-behaviour*
  (or #+clisp :ignore :warn)
  "How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.")

(defvar *compile-file-failure-behaviour*
  (or #+(or mkcl sbcl) :error #+clisp :ignore :warn)
  "How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file?  Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.")


;;; Optimization settings

(defvar *optimization-settings* nil)
(defvar *previous-optimization-settings* nil)
(defun* get-optimization-settings ()
  "Get current compiler optimization settings, ready to PROCLAIM again"
  (let ((settings '(speed space safety debug compilation-speed #+(or cmu scl) c::brevity)))
    #-(or clisp clozure cmu ecl sbcl scl)
    (warn "xcvb-driver::get-optimization-settings does not support your implementation. Please help me fix that.")
    #.`(loop :for x :in settings
         ,@(or #+clozure '(:for v :in '(ccl::*nx-speed* ccl::*nx-space* ccl::*nx-safety* ccl::*nx-debug* ccl::*nx-cspeed*))
               #+ecl '(:for v :in '(c::*speed* c::*space* c::*safety* c::*debug*))
               #+(or cmu scl) '(:for f :in '(c::cookie-speed c::cookie-space c::cookie-safety c::cookie-debug c::cookie-cspeed c::cookie-brevity)))
         :for y = (or #+clisp (gethash x system::*optimize*)
                      #+(or clozure ecl) (symbol-value v)
                      #+(or cmu scl) (funcall f c::*default-cookie*)
                      #+sbcl (cdr (assoc x sb-c::*policy*)))
         :when y :collect (list x y))))
(defun* proclaim-optimization-settings ()
  "Proclaim the optimization settings in *OPTIMIZATION-SETTINGS*"
  (proclaim `(optimize ,@*optimization-settings*))
  (let ((settings (get-optimization-settings)))
    (unless (equal *previous-optimization-settings* settings)
      (setf *previous-optimization-settings* settings))))


;;; Condition control

(defvar *uninteresting-compiler-conditions*
  (append
   #+sbcl
   '(sb-c::simple-compiler-note
     "&OPTIONAL and &KEY found in the same lambda list: ~S"
     sb-int:package-at-variance
     sb-kernel:uninteresting-redefinition
     sb-kernel:undefined-alien-style-warning
     sb-ext:implicit-generic-function-warning
     sb-kernel:lexical-environment-too-complex
     "Couldn't grovel for ~A (unknown to the C compiler)."
     ;; BEWARE: the below four are controversial to include here.
     sb-kernel:redefinition-with-defun
     sb-kernel:redefinition-with-defgeneric
     sb-kernel:redefinition-with-defmethod
     sb-kernel::redefinition-with-defmacro) ; not exported by old SBCLs
   ;;#+clozure '(ccl:compiler-warning)
   '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.")) ;; from closer2mop
  "Conditions that may be skipped while compiling")

(defvar *uninteresting-loader-conditions*
  (append
   '("Overwriting already existing readtable ~S." ;; from named-readtables
     #(#:finalizers-off-warning :asdf-finalizers)) ;; from asdf-finalizers
   #+clisp '(clos::simple-gf-replacing-method-warning))
  "Additional conditions that may be skipped while loading")

(defvar *deferred-warnings* ()
  "Warnings the handling of which is deferred until the end of the compilation unit")

;;;; ----- Filtering conditions while building -----

(defun* call-with-muffled-compiler-conditions (thunk)
  (call-with-muffled-conditions
    thunk *uninteresting-compiler-conditions*))
(defmacro with-muffled-compiler-conditions ((&optional) &body body)
  "Run BODY where uninteresting compiler conditions are muffled"
  `(call-with-muffled-compiler-conditions #'(lambda () ,@body)))
(defun* call-with-muffled-loader-conditions (thunk)
  (call-with-muffled-conditions
   thunk (append *uninteresting-compiler-conditions* *uninteresting-loader-conditions*)))
(defmacro with-muffled-loader-conditions ((&optional) &body body)
  "Run BODY where uninteresting compiler and additional loader conditions are muffled"
  `(call-with-muffled-loader-conditions #'(lambda () ,@body)))

(defun* save-forward-references (forward-references)
  ;; TODO: replace with stuff in POIU
  "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
  #+sbcl
  (loop :for w :in sb-c::*undefined-warnings*
    :for kind = (sb-c::undefined-warning-kind w) ; :function :variable :type
    :for name = (sb-c::undefined-warning-name w)
    :for symbol = (cond
                    ((consp name)
                     (unless (eq kind :function)
                       (error "unrecognized warning ~S not a function?" w))
                     (ecase (car name)
                       ((setf)
                        (assert (and (consp (cdr name)) (null (cddr name))) ())
				  (setf kind :setf-function)
                        (second name))
                       ((sb-pcl::slot-accessor)
                        (assert (eq :global (second name)))
                        (assert (eq 'boundp (fourth name)))
                        (assert (null (nthcdr 4 name)))
                        (setf kind :sb-pcl-global-boundp-slot-accessor)
                        (third name))))
                    (t
                     (assert (member kind '(:function :variable :type)) ())
                     name))
    :for symbol-name = (symbol-name symbol)
    :for package-name = (package-name (symbol-package symbol))
    :collect `(:undefined ,symbol-name ,package-name ,kind) :into undefined-warnings
    :finally (setf *deferred-warnings* undefined-warnings
                   sb-c::*undefined-warnings* nil))
  (when forward-references
    (with-open-file (s forward-references :direction :output :if-exists :supersede)
      (write *deferred-warnings* :stream s :pretty t :readably t)
      (terpri s))))

(defun* call-with-asdf-compilation-unit (thunk &key forward-references)
  (with-compilation-unit (:override t)
    (let ((*deferred-warnings* ())
          #+sbcl (sb-c::*undefined-warnings* nil))
      (multiple-value-prog1
          (with-muffled-compiler-conditions ()
            (funcall thunk))
        (save-forward-references forward-references)))))

(defmacro with-asdf-compilation-unit ((&key forward-references) &body body)
  "Like WITH-COMPILATION-UNIT, but saving forward-reference issues
for processing later (possibly in a different process)."
  `(call-with-xcvb-compilation-unit #'(lambda () ,@body) :forward-references ,forward-references))


;;; from ASDF

(defun* current-lisp-file-pathname ()
  (or *compile-file-pathname* *load-pathname*))

(defun* lispize-pathname (input-file)
  (make-pathname :type "lisp" :defaults input-file))

(defun* compile-file-type (&rest keys)
  "pathname TYPE for lisp FASt Loading files"
  (declare (ignorable keys))
  #-(or ecl mkcl) (load-time-value (pathname-type (compile-file-pathname "foo.lisp")))
  #+(or ecl mkcl) (pathname-type (apply 'compile-file-pathname "foo.lisp" keys)))

(defun* call-around-hook (hook function)
  (call-function (or hook 'funcall) function))

(defun* compile-file* (input-file &rest keys
                                  &key compile-check output-file #+(or ecl mkcl) object-file
                                  &allow-other-keys)
  "This function provides a portable wrapper around COMPILE-FILE.
It ensures that the OUTPUT-FILE value is only returned and
the file only actually created if the compilation was successful,
even though your implementation may not do that, and including
an optional call to an user-provided consistency check function COMPILE-CHECK;
it will call this function if not NIL at the end of the compilation
with the arguments sent to COMPILE-FILE*, except with :OUTPUT-FILE TMP-FILE
where TMP-FILE is the name of a temporary output-file.
It also checks two flags (with legacy british spelling from ASDF1),
*COMPILE-FILE-FAILURE-BEHAVIOUR* and *COMPILE-FILE-WARNINGS-BEHAVIOUR*
with appropriate implementation-dependent defaults,
and if a failure (respectively warnings) are reported by COMPILE-FILE
with consider it an error unless the respective behaviour flag
is one of :SUCCESS :WARN :IGNORE.
On ECL or MKCL, it creates both the linkable object and loadable fasl files.
On implementations that erroneously do not recognize standard keyword arguments,
it will filter them appropriately."
  (let* ((keywords (remove-keys
                    `(:compile-check
                      #+gcl<2.7 ,@'(:external-format :print :verbose)) keys))
         (output-file (apply 'compile-file-pathname* input-file :output-file output-file keywords))
         #+ecl
         (object-file
           (unless (use-ecl-byte-compiler-p)
             (or object-file
                 (compile-file-pathname output-file :type :object))))
         #+mkcl
         (object-file
           (or object-file
               (compile-file-pathname output-file :fasl-p nil)))
         (tmp-file (tmpize-pathname output-file)))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (or #-(or ecl mkcl) (apply 'compile-file input-file :output-file tmp-file keywords)
            #+ecl (apply 'compile-file input-file :output-file
                         (if object-file
                             (list* object-file :system-p t keywords)
                             (list* output-file keywords)))
            #+mkcl (apply 'compile-file input-file :output-file object-file :fasl-p nil keywords))
      (cond
        ((and output-truename
              (flet ((check-flag (flag behaviour)
                       (or (not flag) (member behaviour '(:success :warn :ignore)))))
                (and (check-flag failure-p *compile-file-failure-behaviour*)
                     (check-flag warnings-p *compile-file-warnings-behaviour*)))
              (progn
                #+(or ecl mkcl)
                (when (and #+ecl object-file)
                  (setf output-truename
                        (compiler::build-fasl
                         tmp-file #+ecl :lisp-files #+mkcl :lisp-object-files
                                  (list object-file))))
                (or (not compile-check)
                    (apply compile-check input-file :output-file tmp-file keywords))))
         (delete-file-if-exists output-file)
         (when output-truename
           (rename-file-overwriting-target output-truename output-file)
           (setf output-truename (truename output-file))))
        (t ;; error or failed check
         (delete-file-if-exists output-truename)
         (setf output-truename nil)))
      (values output-truename warnings-p failure-p))))

(defun* compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
  (let* ((keys
           (remove-keys `(#+(and allegro (not (version>= 8 2))) :external-format
                            ,@(unless output-file '(:output-file))) keys)))
    (if (absolute-pathname-p output-file)
        ;; what cfp should be doing, w/ mp* instead of mp
        (let* ((type (pathname-type (apply 'compile-file-type keys)))
               (defaults (make-pathname
                          :type type :defaults (merge-pathnames* input-file))))
          (merge-pathnames* output-file defaults))
        (funcall *output-translation-function*
                 (apply 'compile-file-pathname input-file keys)))))

(defun* load* (x &rest keys &key &allow-other-keys)
  (etypecase x
    ((or pathname string #-(or gcl<2.7 clozure allegro) stream)
     (apply 'load x
            #-gcl<2.7 keys #+gcl<2.7 (remove-keyword :external-format keys)))
    #-(or gcl<2.7 clozure allegro)
    ;; GCL 2.6 can't load from a string-input-stream
    ;; ClozureCL 1.6 can only load from file input stream
    ;; Allegro 5, I don't remember but it must have been broken when I tested.
    (stream ;; make do this way
     (let ((*package* *package*)
           (*readtable* *readtable*)
           (*load-pathname* nil)
           (*load-truename* nil))
       (eval-input x)))))

(defun* load-from-string (string)
  "Portably read and evaluate forms from a STRING."
  (with-input-from-string (s string) (load* s)))

;;; Links FASLs together
(defun* combine-fasls (inputs output)
  #-(or allegro clisp clozure cmu lispworks sbcl scl xcl)
  (error "~A does not support ~S~%inputs ~S~%output  ~S"
         (implementation-type) 'combine-fasls inputs output)
  #+clozure (ccl:fasl-concatenate output inputs :if-exists :supersede)
  #+(or allegro clisp cmu sbcl scl xcl) (concatenate-files inputs output)
  #+lispworks
  (let (fasls)
    (unwind-protect
         (progn
           (loop :for i :in inputs
                 :for n :from 1
                 :for f = (add-pathname-suffix
                           output (format nil "-FASL~D" n))
                 :do #-lispworks-personal-edition (lispworks:copy-file i f)
                     #+lispworks-personal-edition (concatenate-files (list i) f)
                     (push f fasls))
           (ignore-errors (lispworks:delete-system :fasls-to-concatenate))
           (eval `(scm:defsystem :fasls-to-concatenate
                    (:default-pathname ,(pathname-directory-pathname output))
                    :members
                    ,(loop :for f :in (reverse fasls)
                           :collect `(,(namestring f) :load-only t))))
           (scm:concatenate-system output :fasls-to-concatenate))
      (loop :for f :in fasls :do (ignore-errors (delete-file f)))
      (ignore-errors (lispworks:delete-system :fasls-to-concatenate)))))
