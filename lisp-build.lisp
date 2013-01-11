;;;; -------------------------------------------------------------------------
;;;; Support to build (compile and load) Lisp files

(asdf/package:define-package :asdf/lisp-build
  (:recycle :asdf/lisp-build :asdf)
  (:use :common-lisp :asdf/compatibility :asdf/utility :asdf/pathname :asdf/stream :asdf/os :asdf/image)
  (:export
   #:*compile-file-warnings-behaviour* #:*compile-file-failure-behaviour*
   #:*compile-file-function* #:compile-file* #:compile-file-pathname* #:*output-translation-hook*
   #:*optimization-settings*
   #:*uninteresting-conditions* #:*uninteresting-load-conditions*
   #:*fatal-conditions* #:*deferred-warnings*
   #+(or ecl mkcl) #:compile-file-keeping-object
   #:lispize-pathname #:fasl-type #:call-around-hook
   #:*output-translation-hook*
   #:combine-fasls))
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

(defvar *compile-file-function* 'compile-file*
  "Function used to compile lisp files.")

(defvar *output-translation-hook* 'identity)


;;; Optimization settings

(defvar *optimization-settings* nil)
(defvar *previous-optimization-settings* nil)
(defun get-optimization-settings ()
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
(defun proclaim-optimization-settings ()
  "Proclaim the optimization settings in *OPTIMIZATION-SETTINGS*"
  (proclaim `(optimize ,@*optimization-settings*))
  (let ((settings (get-optimization-settings)))
    (unless (equal *previous-optimization-settings* settings)
      (setf *previous-optimization-settings* settings))))


;;; Condition control

(defvar *uninteresting-conditions*
  (append
   #+sbcl
   '(sb-c::simple-compiler-note
     "&OPTIONAL and &KEY found in the same lambda list: ~S"
     sb-int:package-at-variance
     sb-kernel:uninteresting-redefinition
     ;; the below four are controversial to include here;
     ;; however there are issues with the asdf upgrade if they are not present
     sb-kernel:redefinition-with-defun
     sb-kernel:redefinition-with-defgeneric
     sb-kernel:redefinition-with-defmethod
     sb-kernel::redefinition-with-defmacro ; not exported by old SBCLs
     sb-kernel:undefined-alien-style-warning
     sb-ext:implicit-generic-function-warning
     sb-kernel:lexical-environment-too-complex
     "Couldn't grovel for ~A (unknown to the C compiler).")
   ;;#+clozure '(ccl:compiler-warning)
   '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.") ;; from closer2mop
   )
  "Conditions that may be skipped. type symbols, predicates or strings")

(defvar *uninteresting-load-conditions*
  (append
   '("Overwriting already existing readtable ~S." ;; from named-readtables
     #(#:finalizers-off-warning :asdf-finalizers)) ;; from asdf-finalizers
   #+clisp '(clos::simple-gf-replacing-method-warning))
  "Additional conditions that may be skipped while loading. type symbols, predicates or strings")

(defvar *fatal-conditions*
  '(serious-condition)
  "Conditions to be considered fatal during compilation.")

(defvar *deferred-warnings* ()
  "Warnings the handling of which is deferred until the end of the compilation unit")

;;;; ----- Filtering conditions while building -----

(defun match-condition-p (x condition)
  "Compare received CONDITION to some pattern X:
a symbol naming a condition class,
a simple vector of length 2, arguments to find-symbol* with result as above,
or a string describing the format-control of a simple-condition."
  (etypecase x
    (symbol (typep condition x))
    ((simple-vector 2) (typep condition (find-symbol* (svref x 0) (svref x 1) nil)))
    (function (funcall x condition))
    (string (and (typep condition 'simple-condition)
                 #+(or clozure cmu scl) ; Note: on SBCL, always bound, and testing triggers warning
		 (slot-boundp condition
			      #+clozure 'ccl::format-control
			      #+(or cmu scl) 'conditions::format-control)
                 (ignore-errors (equal (simple-condition-format-control condition) x))))))

(defun match-any-condition-p (condition conditions)
  "match CONDITION against any of the patterns of CONDITIONS supplied"
  (loop :for x :in conditions :thereis (match-condition-p x condition)))

(defun uninteresting-condition-p (condition)
  "match CONDITION against any of the patterns of *UNINTERESTING-CONDITIONS*"
  (match-any-condition-p condition *uninteresting-conditions*))

(defun fatal-condition-p (condition)
  "match CONDITION against any of the patterns of *FATAL-CONDITIONS*"
  (match-any-condition-p condition *fatal-conditions*))

(defun call-with-controlled-compiler-conditions (thunk)
  (handler-bind
      ((t
        #'(lambda (condition)
            ;; TODO: do something magic for undefined-function,
            ;; save all of aside, and reconcile in the end of the virtual compilation-unit.
            (cond
              ((uninteresting-condition-p condition)
               (muffle-warning condition))
              ((fatal-condition-p condition)
               (bork condition))))))
    (funcall thunk)))

(defmacro with-controlled-compiler-conditions ((&optional) &body body)
  "Run BODY while suppressing conditions patterned after *UNINTERESTING-CONDITIONS*"
  `(call-with-controlled-compiler-conditions #'(lambda () ,@body)))

(defun call-with-controlled-loader-conditions (thunk)
  (let ((*uninteresting-conditions*
         (append
          *uninteresting-load-conditions*
          *uninteresting-conditions*)))
    (call-with-controlled-compiler-conditions thunk)))

(defmacro with-controlled-loader-conditions ((&optional) &body body)
  "Run BODY while suppressing conditions patterned after *UNINTERESTING-CONDITIONS* plus a few others that don't matter at load-time."
  `(call-with-controlled-loader-conditions #'(lambda () ,@body)))

(defun save-forward-references (forward-references)
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

(defun call-with-asdf-compilation-unit (thunk &key forward-references)
  (with-compilation-unit (:override t)
    (let ((*deferred-warnings* ())
          #+sbcl (sb-c::*undefined-warnings* nil))
      (multiple-value-prog1
          (with-controlled-compiler-conditions ()
            (funcall thunk))
        (save-forward-references forward-references)))))

(defmacro with-asdf-compilation-unit ((&key forward-references) &body body)
  "Like WITH-COMPILATION-UNIT, but saving forward-reference issues
for processing later (possibly in a different process)."
  `(call-with-xcvb-compilation-unit #'(lambda () ,@body) :forward-references ,forward-references))


;;; from ASDF

(defun* lispize-pathname (input-file)
  (make-pathname :type "lisp" :defaults input-file))

(defun* call-around-hook (hook function)
  (funcall (if hook (ensure-function hook) 'funcall) function))

(defun* compile-file* (input-file &rest keys &key compile-check output-file &allow-other-keys)
  (let* ((keywords (remove-keyword :compile-check keys))
         (output-file (apply 'compile-file-pathname* input-file :output-file output-file keywords))
         (tmp-file (tmpize-pathname output-file))
         (status :error))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (apply 'compile-file input-file :output-file tmp-file keywords)
      (cond
        (failure-p
         (setf status *compile-file-failure-behaviour*))
        (warnings-p
         (setf status *compile-file-warnings-behaviour*))
        (t
         (setf status :success)))
      (cond
        ((and (ecase status
                ((:success :warn :ignore) t)
                ((:error nil)))
              (or (not compile-check)
                  (apply compile-check input-file :output-file tmp-file keywords)))
         (delete-file-if-exists output-file)
         (when output-truename
           (rename-file-overwriting-target output-truename output-file)
           (setf output-truename output-file)))
        (t ;; error or failed check
         (delete-file-if-exists output-truename)
         (setf output-truename nil failure-p t)))
      (values output-truename warnings-p failure-p))))

(defun* fasl-type (&rest keys)
  "pathname TYPE for lisp FASt Loading files"
  (declare (ignorable keys))
  #-ecl (load-time-value (pathname-type (compile-file-pathname "foo.lisp")))
  #+ecl (pathname-type (apply 'compile-file-pathname "foo.lisp" keys)))

(defun* compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
  (if (absolute-pathname-p output-file)
      ;; what cfp should be doing, w/ mp* instead of mp
      (let* ((type (pathname-type (apply 'fasl-type keys)))
             (defaults (make-pathname
                        :type type :defaults (merge-pathnames* input-file))))
        (merge-pathnames* output-file defaults))
      (funcall *output-translation-hook*
               (apply 'compile-file-pathname input-file
                      (if output-file keys (remove-keyword :output-file keys))))))

;;; ECL and MKCL support for COMPILE-OP / LOAD-OP
;;;
;;; In ECL and MKCL, these operations produce both
;;; FASL files and the object files that they are built from.
;;; Having both of them allows us to later on reuse the object files
;;; for bundles, libraries, standalone executables, etc.
;;;
;;; This has to be in asdf.lisp and not asdf-ecl.lisp, or else it becomes
;;; a problem for asdf on ECL to compile asdf-ecl.lisp after loading asdf.lisp.
;;;
#+(or ecl mkcl)
(progn
  (setf *compile-file-function* 'compile-file-keeping-object)

  (defun* compile-file-keeping-object (input-file &rest keys &key output-file &allow-other-keys)
    (#+ecl if #+ecl (use-ecl-byte-compiler-p) #+ecl (apply 'compile-file* input-file keys)
     #+mkcl progn
     (multiple-value-bind (object-file flags1 flags2)
         (apply 'compile-file* input-file
                #+ecl :system-p #+ecl t #+mkcl :fasl-p #+mkcl nil
                :output-file (compile-file-pathname
                              output-file . #+ecl (:type :object) #+mkcl (:fasl-p nil)) keys)
       (values (and object-file
                    (compiler::build-fasl
                     output-file #+ecl :lisp-files #+mkcl :lisp-object-files (list object-file))
                    object-file)
               flags1
               flags2)))))

(defun* combine-fasls (inputs output)
  #-(or allegro clisp clozure cmu lispworks sbcl scl xcl)
  (declare (ignore inputs output))
  #-(or allegro clisp clozure cmu lispworks sbcl scl xcl)
  (error "~S is not supported on ~A" 'combine-fasls (implementation-type))
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

