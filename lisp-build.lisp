;;;; -------------------------------------------------------------------------
;;;; Support to build (compile and load) Lisp files

(asdf/package:define-package :asdf/lisp-build
  (:recycle :asdf/lisp-build :asdf)
  (:use :common-lisp :asdf/utility :asdf/pathname :asdf/os)
  (:fmakunbound #:compile-file*)
  (:export
   #:*compile-file-warnings-behaviour* #:*compile-file-failure-behaviour*
   #:*compile-file-function* #:compile-file* #:compile-file-pathname*
   #+(or ecl mkcl) #:compile-file-keeping-object
   #:lispize-pathname #:fasl-type #:call-around-hook
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
      (funcall (find-symbol* :apply-output-translations :asdf/output-translations)
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


