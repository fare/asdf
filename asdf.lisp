;;; the problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file

(defpackage #:asdf
  (:export #:defsystem #:oos #:find-system #:run-shell-command
	   #:find-component		; miscellaneous
	   
	   #:compile-op #:load-op #:test-system-version
	   #:operation			; operations
	   #:feature			; sort-of operation
	   
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
	   )
  (:use "CL"))

(in-package #:asdf)

(proclaim '(optimize (debug 3)))
(declaim (optimize (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility stuff

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problems

(define-condition system-definition-error (error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
	     (apply #'format s (format-control c) (format-arguments c)))))

(defun sysdef-error (format &rest arguments)
  (error 'system-definition-error :format-control format :format-arguments arguments))

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
   (in-order-to :initform nil :initarg :in-order-to)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)))

(defgeneric component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defmethod component-pathname  ((component component))
  (let ((*default-pathname-defaults* *component-parent-pathname*))
    (merge-pathnames (component-relative-pathname component))))


(defmethod print-object ((c component) (stream stream))
  (print-unreadable-object (c stream :type t :identity t)
    (ignore-errors
      (prin1 (component-name c) stream))))

(defclass module (component)
  ((components :accessor module-components :initarg :components)
   ;; what to do if we can't satisfy a dependency of one of this module's
   ;; components.  This allows a limited form of conditional processing
   (if-component-dep-fails :initform :fail
			   :accessor module-if-component-dep-fails
			   :initarg :if-component-dep-fails)
   (default-component-class :accessor module-default-component-class
     :initform 'cl-source-file :initarg :default-component-class)))

(defmethod component-relative-pathname ((component module))
  (let ((*default-pathname-defaults* *component-parent-pathname*))
    (or (slot-value component 'relative-pathname)
	(make-pathname :directory `(:relative ,(component-name component))))))

(defgeneric find-component (module name &optional version)
  (:documentation "Finds the component with name NAME present in the
MODULE module; if MODULE is nil, then the component is assumed to be a
system."))

(defmethod find-component ((module module) name &optional version)
  (declare (ignore version))
  (if (slot-boundp module 'components)
      (find name (module-components module)
	    :test #'equal :key #'component-name)))

;;; a component with no parent is a system
(defmethod find-component ((module (eql nil)) name &optional version)
  (declare (ignore version))
  (find-system name))

(defclass source-file (component) ())

(defclass cl-source-file (source-file) ())

;;; a reversible registry that lets us query class <-> extension

(defvar *known-extensions*
  '(("lisp" . cl-source-file)
    ("c" . c-source-file)
    ("java" . java-source-file)))

(defmethod component-relative-pathname ((component source-file))
  (let ((*default-pathname-defaults* *component-parent-pathname*))
    (or (slot-value component 'relative-pathname)
	(make-pathname :name (component-name component)
		       :type (car (rassoc (class-name (class-of component))
					  *known-extensions*))))))

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

(defgeneric perform (operation component))
(defgeneric operation-done-p (operation component))
(defgeneric explain (operation component))
(defgeneric output-files (operation component))

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
    raw-form
    #+nil
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
  (labels ((do-one-dep (required-op required-c required-v)
	     (let ((op (if (subtypep (type-of operation) required-op)
			   operation
			   (make-instance required-op :force
					  (operation-forced-p operation))))
		   (dep-c (or (find-component
			       *component-parent* required-c required-v)
			      (error 'missing-dependency :required-by c
				     :version required-v
				     :requires required-c))))
	       (traverse op dep-c function)))
	   (do-every-dep (op deps)
	     (dolist (d deps)
	       (do-dep op  d)))
	   (do-first-dep (op deps)
	     (block found
	       (dolist (d deps)
		 (handler-case
		     (do-dep op d)
		   (missing-dependency (c) (return-from found nil)))
		 (error 'missing-dependency
			:version nil :required-by c :requires deps))))
	   (do-dep (op dep)
	     (when (eq op 'feature)
	       (return-from do-dep
		 (or (member (car dep) *features*)
		     (error 'missing-dependency :required-by c
			    :requires (car dep) :version nil))))
	     (cond 
	       ((consp dep)
		(case (car dep)
		  (and (do-every-dep op (cdr dep)))
		  (or (do-first-dep op (cdr dep)))
		  (version
		   (destructuring-bind (ignore name version-object)
		       (do-one-dep op name version-object)))
		  ;; if we had a list with unrecognised car, assume 'and'
		  (t (do-every-dep op dep))))
	       (t (do-one-dep op dep nil)))))
    ;; dependencies
    (if (component-visiting-p operation c)
	(error 'circular-dependency :components (list c)))
    (setf (visiting-component operation c) t)
    (loop for (required-op . deps) in (component-depends-on operation c)
	  do (do-dep required-op deps))
    ;; constituent bits
    (when (typep c 'module)
      (let ((*component-parent-pathname* (component-pathname c))
	    (*component-parent* c)
	    (at-least-one nil)
	    (error nil))
	(loop for kid in (module-components c)
	      do (handler-case
		     (traverse operation kid function)		   
		   (missing-dependency (condition)
		     (if (eq (module-if-component-dep-fails c) :fail)
			 (error condition))
		     (setf error condition))
		   (:no-error (c)
		     (setf at-least-one t))))
	(when (and (eq (module-if-component-dep-fails c) :try-next)
		   (not at-least-one))
	  (error error))))
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
      (visit-component operation c))))

(defmethod perform ((operation operation) (c source-file))
  (sysdef-error
   "Required method PERFORM not implemented for operation ~A, component ~A"
   (class-of operation) (class-of c)))

(defmethod perform ((operation operation) (c module))
  nil)

(defmethod explain ((operation operation) (component component))
  (format *trace-output* "~&;;; ~A on ~A~%"
	  operation component))

(defmethod output-files ((operation operation) (c component))
  (sysdef-error "Required method OUTPUT-FILES not implemented for operation ~A"
	 (class-of operation)))

;;; compile-op

(defclass compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (fail-on-error-p :initarg :fail-on-error
		    :accessor operation-fail-on-error-p :initform t)
   (fail-on-warning-p :initarg :fail-on-warning
		      :accessor operation-fail-on-warning-p :initform nil)))

;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((operation compile-op) (c cl-source-file))
  (let ((source-file (component-pathname c)))
    (multiple-value-bind (output warnings-p failure-p)
	(compile-file source-file
		      :output-file (car (output-files operation c)))
      (when (and warnings-p (operation-fail-on-warning-p operation))
	(error 'compile-warned :component c :operation operation))
      (when (and failure-p (operation-fail-on-error-p operation))
	(error 'compile-failed :component c :operation operation)))))

(defmethod output-files ((operation compile-op) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c))))

;;; load-op

(defclass load-op (operation) ())

(defmethod perform ((o load-op) (c cl-source-file))
  (let ((co (make-instance 'compile-op)))
    (map nil #'load (output-files co c))))

(defmethod output-files ((operation load-op) (c component))
  nil)

(defmethod component-depends-on ((operation load-op) (c source-file))
  (cons (list 'compile-op (component-name c))
        (call-next-method)))


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

(defun register-system (name system)
  (format t "Registering ~A as ~A ~%" system name)
  (setf (gethash (if (symbolp name) (symbol-name name) name) *defined-systems*)
	(cons (get-universal-time) system)))

(defun system-registered-p (name)
  (gethash (if (symbolp name) (symbol-name name) name) *defined-systems*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax

(defmacro defsystem (name &body options)
  (destructuring-bind (&key pathname &allow-other-keys) options
    `(progn
      ;; system must be registered before we parse the body, otherwise
      ;; we recur when trying to find an existing system of the same name
      ;; to reuse options (e.g. pathname) from
      (let ((s (system-registered-p ',name)))
	(if s
	    (setf (car s) (get-universal-time))
	    (register-system (quote ,name)
			     (make-instance 'module :name ',name))))
      (parse-component-form nil (apply
				 #'list
				 :module ',name
				 :pathname
				 (or ,pathname
				     (pathname-sans-name+type *load-truename*)
				     *default-pathname-defaults*)
				 ',options)))))


(defun class-for-type (parent type)
  (let ((class (find-class (intern (symbol-name type) *package*) nil)))
    (or class
	(and (eq type :file)
	     (or (module-default-component-class parent)
		 (find-class 'cl-source-file)))
	(sysdef-error "Don't recognize component type ~A" type))))

(defun maybe-add-tree (tree op1 op2 c)
  "Add the node C at /OP1/OP2 in TREE, unless it's there already.
Returns the new tree (which probably shares structure with the old one)"
  (let ((first-op-tree (assoc op1 tree)))
    (if first-op-tree
	(progn
	  (aif (assoc op2 (cdr first-op-tree))
	       (if (find c (cdr it))
		   nil
		   (setf (cdr it) (cons c (cdr it))))
	       (setf (cdr first-op-tree)
		     (acons op2 (list c) (cdr first-op-tree))))
	  tree)
	(acons op1 (list (list op2 c)) tree))))
		
(defun union-of-dependencies (&rest deps)
  (let ((new-tree nil))
    (dolist (dep deps)
      (dolist (op-tree dep)
	(dolist (op  (cdr op-tree))
	  (dolist (c (cdr op))
	    (setf new-tree
		  (maybe-add-tree new-tree (car op-tree) (car op) c))))))
    new-tree))

;;; ew
#.(defparameter *option-names*
    '(components pathname default-component-class
      perform explain output-files operation-done-p
      depends-on serialize in-order-to))

(defun remove-keys (key-names args)
  (loop for ( name val ) on args by #'cddr
	unless (member (symbol-name name) key-names 
		       :key #'symbol-name :test 'equal)
	append (list name val)))

(defun parse-component-form (parent options)
  (destructuring-bind
	#.`(type name &rest rest &key ,@*option-names*
	    &allow-other-keys) options
	    (declare (ignore serialize))
	    ;; XXX add dependencies for serialized subcomponents
	    (let* ((other-args (remove-keys *option-names* rest))
		   (ret
		    (or (find-component parent name)
			(make-instance (class-for-type parent type)))))
	      (apply #'reinitialize-instance
		     ret
		     :name name
		     :pathname pathname
		     :in-order-to (union-of-dependencies
				   in-order-to
				   `((compile-op (load-op ,@depends-on))))
		     other-args)
	      (when (typep ret 'module)
		(setf (module-default-component-class ret)
		      (or default-component-class
			  (and (typep parent 'module)
			       (module-default-component-class parent)))))
	      (when components
		(setf (module-components ret)
		      (mapcar (lambda (x) (parse-component-form ret x)) components)))
	      (loop for (n v) in `((perform ,perform) (explain ,explain)
				   (output-files ,output-files)
				   (operation-done-p ,operation-done-p))
		    do (map 'nil
			    ;; this is inefficient as most of the stored
			    ;; methods will not be for this particular gf n
			    ;; But this is hardly performance-critical
			    (lambda (m) (remove-method (symbol-function n) m))
			    (component-inline-methods ret))
		    when v
		    do (destructuring-bind (op qual (o c) &body body) v
			 (pushnew
			  (eval `(defmethod ,n ,qual ((,o ,op) (,c (eql ,ret)))
				  ,@body))
			  (component-inline-methods ret))))
	      ret)))


;;; optional extras

;;; run-shell-command functions for other lisp implementations will be
;;; gratefully accepted, if they do the same thing.  If the docstring
;;; is ambiguous, send a bug report

#+sbcl
(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *trace-output*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format *trace-output* "; $ ~A~%" command)
    (sb-impl::process-exit-code
     (sb-ext:run-program  
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *trace-output*))))


