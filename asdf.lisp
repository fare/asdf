;;; the problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file

(defpackage #:asdf
  (:export #:defsystem #:oos #:find-system #:run-shell-command
	   #:find-component		; miscellaneous
	   
	   #:compile-system #:load-system #:test-system-version
	   #:operation			; operations
	   
	   #:output-files #:perform	; operation methods
	   #:operation-done-p #:explain
	   
	   #:component #:module #:source-file 
	   #:c-source-file #:cl-source-file #:java-source-file
	   #:module			; components
	   #:unix-dso
	   
	   #:module-components		; component accessors
	   #:component-pathname
	   #:component-relative-pathname
	   #:component-name
	   #:component-version

	   #:component-depends-on
	   
	   #:*component-parent-pathname* ; variables
	   #:*central-registry*
	   
	   #:operation-error #:compile-failed #:compile-warned
	   #:system-definition-error #:system-not-found
	   #:circular-dependency	; errors
	   ))

(in-package #:asdf)

(proclaim '(optimize (debug 3)))
(declaim (optimize (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problems

(define-condition system-definition-error (error)
  ())
(define-condition circular-dependency (system-definition-error)
  ((components :initarg :components)))
(define-condition missing-dependency (system-definition-error)
  ((requires :initarg :requires :reader requires)
   (required-by :initarg :required-by :reader required-by))
  (:report (lambda (c s)
	     (format s "unsatisfied dependency: component ~S requires ~S"
		     (required-by c) (requires c)))))

(define-condition system-not-found (system-definition-error)
  ((name :initform "(unnamed)" :reader system-name  :initarg :name))
  (:report (lambda (c s)
	     (format s "System ~S not found"
		     (system-name c)))))

(define-condition operation-error (error)
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
	     (format s "Erred while invoking ~A on ~A"
		     (error-operation c) (error-component c)))))
(define-condition compile-failed (operation-error) ())
(define-condition compile-warned (operation-error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; components

(defclass component ()
  ((name :type string :accessor component-name :initarg :name :documentation
	 "Component name, restricted to portable pathname characters")
   (version :accessor component-version :initarg :version)
   ;; the defsystem syntax allows us to define EQL methods with our
   ;; components.  We must keep track of them all so we can get rid of
   ;; them if need be when the defsystem form is re-evaluated
   (inline-methods :initform nil :initarg :inline-methods)
   (in-order-to :initform nil :initarg :in-order-to)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   ;; (:pathname is actually a really bad choice of name for this slot,
   ;; because quite often that's not what we actually use it for)
   (pathname :initarg :pathname)))

(defmethod string-unix-common-casify (string &key (start 0) end)
  "Converts a string assumed local to a Unix filesystem into its
:common :case partner."
  (unless end
    (setf end (length string)))
  (let ((result (copy-seq string)))
    (cond
      ((every (lambda (x) (or (upper-case-p x) (not (alpha-char-p x)))) (subseq string start end))
       (nstring-downcase result :start start :end end))
      ((every (lambda (x) (or (lower-case-p x) (not (alpha-char-p x)))) (subseq string start end))
       (nstring-upcase result :start start :end end))
      (t result))))


(defmethod component-pathname  ((component component))
  (let ((*default-pathname-defaults* *component-parent-pathname*))
    (if (slot-boundp component 'pathname)
	(let ((p (slot-value component 'pathname)))
	  (cond ((pathnamep p)
		 (merge-pathnames p))
		((and (stringp p) (> (length p) 0))
		 (merge-pathnames p))
		((and (stringp p) (= (length p) 0))
		 *component-parent-pathname*)
		(t
		 (warn ":pathname argument to ~A is not a pathname designator.  Ignoring it" component)
		 (merge-pathnames (component-relative-pathname component)))))
	(merge-pathnames (component-relative-pathname component)))))
	   

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity t)
			   (ignore-errors
			     (prin1 (component-name c) stream))))

(defclass module (component)
  ((components :accessor module-components :initarg :components)
   (default-component-class :accessor module-default-component-class
     :initform 'cl-source-file :initarg :default-component-class)))

(defmethod component-relative-pathname ((component module))
  (make-pathname :directory `(:relative ,(component-name component))
		 :host (pathname-host *component-parent-pathname*)))

(defmethod find-component ((module module) name)
  (if (slot-boundp module 'components)
      (find name (module-components module)
	    :test #'equal :key #'component-name)))

;;; a component with no parent is a system
(defmethod find-component ((module (eql nil)) name)
  (find-system name))

(defclass source-file (component) ())

(defclass cl-source-file (source-file) ())

;;; a reversible registry that lets us query class <-> extension

(defvar *known-extensions*
  '(("lisp" . cl-source-file)
    ("c" . c-source-file)
    ("java" . java-source-file)))

(defmethod component-relative-pathname ((component source-file))
  (make-pathname :name (string-unix-common-casify (component-name component))
		 :type (string-unix-common-casify (car (rassoc (class-name (class-of component))
				    *known-extensions*)))
		 :case :common))

(defclass c-source-file (source-file) ())
(defclass java-source-file (source-file) ())

;;; XXX we don't need the first of these if we have the second, probably
(defvar *component-parent-pathname*)
(defvar *component-parent*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operations

;;; one of these is instantiated whenever (operate ) is called

(defclass operation ()
  ((forced-p :initform nil :initarg :force :accessor operation-forced-p )))

(defgeneric perform (operation system))
(defgeneric operation-done-p (operation system))
(defgeneric explain (operation system))
(defgeneric output-files (operation system))

(defvar *visited-nodes* nil)
(defvar *visiting-nodes* nil)

(defun node-for (o c)
  (cons (class-name (class-of o)) c))

(defmethod visit-component ((o operation) (c component))
  (pushnew (node-for o c) *visited-nodes* :test 'equal))

(defmethod component-visited-p ((o operation) (c component))
  (member (node-for o c) *visited-nodes* :test 'equal))

(defmethod (setf visiting-component) (new-value (o operation) (c component))
  (let ((node (node-for o c)))
    (if new-value
	(pushnew node *visiting-nodes* :test 'equal)
	(setf *visiting-nodes* (remove node *visiting-nodes* :test 'equal)))))

(defmethod component-visiting-p ((o operation) (c component))
  (let ((node (cons o c)))
    (member node *visiting-nodes* :test 'equal)))

;;; this needs a new name.  should be operation-needs-doing-p
(defmethod operation-done-p ((o operation) (c source-file))
  (let ((binaries (output-files o c))
	(source-write-date
	 (and (probe-file (component-pathname c))
	      (file-write-date (component-pathname c)))))
    (and source-write-date binaries 
	 (every (lambda (b)
		  (and (probe-file b)
		       (> (file-write-date b) source-write-date)))
		binaries))))

(defmethod operation-done-p ((o operation) (c component))
  ;;; can't tell easily
  nil)

(defgeneric component-depends-on (operation component))

(defmethod component-depends-on ((o operation) (c component))
  (let ((raw-form
	 (cdr (assoc (class-name (class-of o))
		     (slot-value c 'in-order-to)))))
    (loop for (op . components) in raw-form
	  append (mapcar (lambda (x) (list op x)) components))))


;;; So you look at this code and think "why isn't it a bunch of
;;; methods".  And the answer is, because standard method combination
;;; runs :before methods most->least-specific, which is back to front
;;; for our purposes.  And CLISP doesn't have non-standard method
;;; combinations, so let's keep it simple and aspire to portability

;;; we enforce that function is a symbol to allow us to specialize on
;;; (eql 'perform) and (eql 'explain) for :before and :after
(defmethod traverse ((operation operation) (c component) (function symbol))
  ;; dependencies
  (if (component-visiting-p operation c)
      (error 'circular-dependency :components (list c)))
  (setf (visiting-component operation c) t)
  (loop for (prereq-op  prereq-c) in
	(component-depends-on operation c)
	;; this operation instantiation thing sucks somewhat, as we don't
	;; transfer arguments in any meaningful way.  if compile calls
	;; load calls compile, how do we still have the original proclamations?
	do (let ((op (if (subtypep (type-of operation) prereq-op)
			 operation
			 (make-instance prereq-op :force
					(operation-forced-p operation))))
		 (dep-c (or (find-component *component-parent* prereq-c)
			    (error 'missing-dependency :required-by c
				   :requires prereq-c))))
	     (traverse op dep-c function)))
  ;; constituent bits
  (when (typep c 'module)
    (let ((*component-parent-pathname* (component-pathname c))
	  (*component-parent* c))
      (mapc (lambda (c) (traverse operation c function))
	    (module-components c))))
  ;; now the thing itself
  (unless (component-visited-p operation c)
    (if (or (operation-forced-p operation)
	    (not (operation-done-p operation c)))
	(loop
	 (restart-case 
	     (progn (funcall function operation c)
		    (return))
	   (retry-component ())
	   (skip-component () (return)))))
    (setf (visiting-component operation c) nil)	      
    (visit-component operation c)))

(defmethod perform ((operation operation) (c source-file))
  (error
   "Required method PERFORM not implemented for operation ~A, component ~A"
   (class-of operation) (class-of c)))

(defmethod perform ((operation operation) (c module))
  nil)

(defmethod explain ((operation operation) (component component))
  (format *trace-output* "~&;;; ~A on ~A~%"
	  operation component))

(defmethod output-files ((operation operation) (c component))
  (error "Required method OUTPUT-FILES not implemented for operation ~A"
	 (class-of operation)))

;;; compile-system

(defclass compile-system (operation)
  ((proclamations :initarg :proclamations :accessor compile-system-proclamations :initform nil)
   (fail-on-error-p :initarg :fail-on-error
		    :accessor operation-fail-on-error-p :initform t)
   (fail-on-warning-p :initarg :fail-on-warning
		      :accessor operation-fail-on-warning-p :initform nil)))

(defmethod perform ((operation compile-system) (c cl-source-file))
  (let ((source-file (component-pathname c)))
    (multiple-value-bind (output warnings-p failure-p)
	(compile-file source-file)
      (when (and warnings-p (operation-fail-on-warning-p operation))
	(error 'compile-warned :component c :operation operation))
      (when (and failure-p (operation-fail-on-error-p operation))
	(error 'compile-failed :component c :operation operation)))))

(defmethod output-files ((operation compile-system) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c))))

;;; load-system

(defclass load-system (operation) ())

(defmethod component-depends-on ((operation load-system) (c component))
  (cons
   (list 'compile-system (component-name c))
   (call-next-method)))

(defmethod perform ((o load-system) (c cl-source-file))
  (let ((co (make-instance 'compile-system)))
    (load (car (output-files co c)))))

(defmethod output-files ((operation load-system) (c component))
  nil)
  
;;; test-system-version

(defclass test-system-version (operation)
  ((minimum :initarg :minimum :initform ""
	    :accessor test-system-version-minimum)))

;;; with apologies to christophe rhodes ...
(defun split (string &optional max (ws '(#\Space #\Tab)))
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
	(when (and max (>= words (1- max)))
	  (return (cons (subseq string start) list)))
	(setf end (position-if #'is-ws string :start start))
	(push (subseq string start end) list)
	(incf words)
	(unless end (return list))
	(setf start (1+ end)))))))

(defmethod perform ((operation test-system-version) (c component))
  (let ((x (mapcar #'parse-integer
		   (split (component-version c) nil '(#\.))))
	(y (mapcar #'parse-integer
		   (split
		    (test-system-version-minimum operation) nil '(#\.)))))
    (labels ((bigger (x y)
	       (cond ((not y) t)
		     ((not x) nil)
		     ((> (car x) (car y)) t)
		     ((= (car x) (car y))
		      (bigger (cdr x) (cdr y))))))
      (and (= (car x) (car y))
	   (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoking operations

(defun oos (operation-class system &rest args)
  (let ((op (apply #'make-instance operation-class args))
	(*component-parent-pathname* *default-pathname-defaults*)
	(*visiting-nodes* nil)
	(*visited-nodes* nil)
	(*component-parent* nil)
	(system (if (typep system 'component) system (find-system system))))
    (traverse op system 'perform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding systems

(defvar *defined-systems* (make-hash-table :test 'equal))
(defparameter *central-registry*
  '(*default-pathname-defaults*
    "/home/dan/src/sourceforge/cclan/asdf/systems/"
    "telent:asdf;systems;"))

(defun find-system (name)
  (let* ((name (if (symbolp name) (symbol-name name) name))
	 (in-memory (gethash name *defined-systems*))
	 (on-disk
	  (dolist (dir *central-registry*)
	    (let* ((defaults (eval dir))
		   (file (merge-pathnames
			  (make-pathname
			   :case :common :name name :type "SYSTEM"
			   :defaults defaults)
			  defaults)))
	      (if (probe-file file) (return file)))) ))
    (when (and on-disk
	       (or (not in-memory)
		   (< (car in-memory) (file-write-date on-disk))))
      (let ((*package* (make-package (gensym (package-name #.*package*))
				     :use '("CL" "ASDF"))))
	(format t ";;; Loading system definition from ~A into ~A~%"
		on-disk *package*)
	(load on-disk)))
    (let ((in-memory (gethash name *defined-systems*)))
      (if in-memory
	  (progn (if on-disk (setf (car in-memory) (file-write-date on-disk)))
		 (cdr in-memory))
	  (error 'system-not-found :name name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax

(defmacro defsystem (name &body options)
  (let ((name (if (symbolp name) (symbol-name name) name)))
    (multiple-value-bind (initargs bindings)
        (process-option-list options)
      ;; this macro is called (sic) during executing of find-system, so
      ;; we had better not call find-system recursively
      ;;
      ;; asdf:component is explicitly here to be shadowed later on.
      `(let ((component (or (cdr (gethash ,name *defined-systems*))
		    (make-instance 'module :name ,name  ))))
	(setf (gethash ,name *defined-systems*) (cons 0 component))
	(let (,@bindings) ; yes, I know this is the same as let ,bindings
	  (reinitialize-instance component :name ,name ,@initargs))))))

(defmethod process-option-list (options)
  (loop for (name value) on options by #'cddr
        for (i b) = (multiple-value-list (process-option name value))
	append i into initargs
        if b append b into bindings
        finally (return (values initargs bindings))))

;;; process-option returns as its first value a list of initargs that
;;; eventually gets appended to a call to reinitialize instance; its
;;; optional second value is a list of binding clauses suitable for a
;;; let that may be referred to in the initargs.
(defmethod process-option (option  value)
  (list option value))

#|
source-file components defined with (:file "a-string") or "a-string"
will have the string parsed into name and type as if it were a
filename, and an instance of the appropriate source-file subclass
created.  If a type is not provided, it will default to the parent's
default constituent type.
|#

(defun create-instance-for-component (keyword name args)
  (multiple-value-bind (initargs bindings)
      (process-option-list args)
    `(let* ((name-bits (split ,name 2 '(#\.)))
           (name (car name-bits))
           (extension (second name-bits))
           (class
            (cond ((eq ,keyword :file)
                   (if extension
                       (cdr (assoc extension *known-extensions* :test 'equal))
                       (module-default-component-class component)))
                  ((eq ,keyword :module) 'module)
                  (t (intern (symbol-name ,keyword) *package*)))))
      ;; here's where we use asdf:component. This is probably a
      ;; documentable feature of the implementation (`option
      ;; processing may use asdf:component to refer to the parent
      ;; thingy; and MUST bind asdf:component to the current thingy
      ;; before doing recursive processing').
      (let ((component (or (find-component component name)
			   (make-instance class :name name))))
	(let (,@bindings)
	  (reinitialize-instance component :name name ,@initargs)
	  component)))))

(defmethod process-option ((option (eql :components)) value)
  ;; we don't want to shadow asdf::cs
  (let ((cs (gensym "CS")))
    (values
     (list :components cs)
     (list (list cs
		 (list* 'list
			(mapcar
			 (lambda (i)
			   (if (consp i)
			       (create-instance-for-component (first i) (second i) (cddr i))
			       (create-instance-for-component :file i nil)))
			 value)))))))

;;; optional extras

;;; run-shell-command functions for other lisp implementations will be
;;; gratefully accepted, if they do the same thing.  If the docstring
;;; is ambiguous, send a bug report

#+sbcl
(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and execute the result using a Bourne-compatible shell, with output to *trace-output*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format *trace-output* "; $ ~A~%" command)
    (sb-impl::process-exit-code
     (sb-ext:run-program  
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *trace-output*))))

(defmethod process-option ((option (eql :perform)) value)
  (destructuring-bind
	(op-specializer combination (op c) &body body)
      value
    (values
     nil
     `((#:ignore
	(defmethod perform ,combination ((,op ,op-specializer) (,c (eql component)))
	  ,@body))))))

(defmethod process-option ((option (eql :explain)) value)
  (destructuring-bind
	(op-specializer combination (op c) &body body)
      value
    (values
     nil
     `((#:ignore
	(defmethod explain ,combination ((,op ,op-specializer) (,c (eql component)))
	  ,@body))))))

;;; mk-compatibility
(defmethod process-option ((option (eql :source-pathname)) value)
  (list :pathname value))

(defmethod process-option ((option (eql :source-extension)) value)
  ;; we currently ignore this; arguably we shouldn't.
  nil)

(defmethod process-option ((option (eql :binary-pathname)) value)
  ;; we currently ignore this
  nil)

(defmethod process-option ((option (eql :binary-extension)) value)
  ;; we currently ignore this
  nil)

(defmethod process-option ((option (eql :depends-on)) value)
  (list :in-order-to `'((compile-system (load-system ,@value)))))

;;; initially-do (and finally-do) may need to be moved out of
;;; mk-compatibility (or maybe renamed first...)
(defmethod process-option ((option (eql :initially-do)) value)
  (let ((op (gensym "OPERATION"))
	(c (gensym "COMPONENT"))
	(f (gensym "FUNCTION")))
    (values
     nil ; no initargs needed -- functionality is in the method.
     `((#:ignore
	;; look, ma! No explicit coercion or compilation needed! Also
	;; note that we are using the asdf:component thing.
	(defmethod traverse :before ((,op compile-system) (,c (eql component)) (,f (eql 'perform)))
	  ,value))))))
		  
(defmethod process-option ((option (eql :finally-do)) value)
  (let ((op (gensym "OPERATION"))
	(c (gensym "COMPONENT"))
	(f (gensym "FUNCTION")))
    (values
     nil ; no initargs needed
     `((#:ignore
	(defmethod traverse :after ((,op load-system) (,c (eql component)) (,f (eql 'perform)))
	  ,value))))))

(defmethod process-option ((option (eql :load-only)) value)
  (let ((op (gensym "OPERATION"))
	(c (gensym "COMPONENT")))
    (when value
      (values
       nil
       `((#:ignore
	  (defmethod perform ((,op compile-system) (,c (eql component)))
	    nil))
	 (#:ignore
	  (defmethod output-files ((,op compile-system) (,c (eql component)))
	    (list (component-pathname ,c)))))))))