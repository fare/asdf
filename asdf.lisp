;;; the problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file

(defpackage #:asdf
  (:export #:defsystem #:oos #:operate #:find-system #:run-shell-command
	   #:system-definition-pathname #:find-component ; miscellaneous
	   
	   #:compile-op #:load-op #:test-system-version
	   #:operation			; operations
	   #:feature			; sort-of operation
	   #:version			; metaphorically sort-of an operation
	   
	   #:output-files #:perform	; operation methods
	   #:operation-done-p #:explain
	   
	   #:component #:module #:source-file 
	   #:c-source-file #:cl-source-file #:java-source-file
	   #:static-file
	   #:doc-file
	   #:html-file
	   #:text-file
	   #:source-file-type
	   #:module			; components
	   #:unix-dso
	   
	   #:module-components		; component accessors
	   #:component-pathname
	   #:component-relative-pathname
	   #:component-name
	   #:component-version
	   #:component-parent
	   
	   #:component-depends-on
	   
	   ;#:*component-parent-pathname* 
	   #:*central-registry*		; variables
	   
	   #:operation-error #:compile-failed #:compile-warned
	   #:system-definition-error 
	   #:missing-component
	   #:missing-dependency
	   #:circular-dependency	; errors
	   )
  (:use :cl))

(in-package #:asdf)

(proclaim '(optimize (debug 3)))
(declaim (optimize (debug 3)))

(defvar  *compile-file-warnings-behaviour* :warn)
(defvar  *compile-file-failure-behaviour* :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility stuff

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes, condiitons

(define-condition system-definition-error (error) ())
(define-condition formatted-system-definition-error (system-definition-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
	     (apply #'format s (format-control c) (format-arguments c)))))

(define-condition circular-dependency (system-definition-error)
  ((components :initarg :components)))

(define-condition missing-component (system-definition-error)
  ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
   (version :initform nil :reader missing-version :initarg :version)
   (parent :initform nil :reader missing-parent :initarg :parent)))

(define-condition missing-dependency (missing-component)
  ((required-by :initarg :required-by :reader missing-required-by)))

(define-condition operation-error (error)
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
	     (format s "Erred while invoking ~A on ~A"
		     (error-operation c) (error-component c)))))
(define-condition compile-failed (operation-error) ())
(define-condition compile-warned (operation-error) ())

(defclass component ()
  ((name :type string :accessor component-name :initarg :name :documentation
	 "Component name, restricted to portable pathname characters")
   (version :accessor component-version :initarg :version)
   (in-order-to :initform nil :initarg :in-order-to)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil)
   (parent :initarg :parent :initform nil :reader component-parent)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties :accessor component-properties :initarg :properties)))
  
;;;; methods: conditions

(defmethod print-object ((c missing-dependency) s)
  (call-next-method)
  (format s ", required by ~A" (missing-required-by c)))

(defun sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error :format-control format :format-arguments arguments))

;;;; methods: components

(defmethod print-object ((c missing-component) s)
  (format s "Component ~S not found" (missing-requires c))
  (when (missing-version c)
    (format s " or does not match version ~A" (missing-version c)))
  (when (missing-parent c)
    (format s " in ~A" (component-name (missing-parent c)))))

(defgeneric component-system (component)
  (:documentation "Find the top-level system containing COMPONENT"))
  
(defmethod component-system ((component component))
  (aif (component-parent component)
       (component-system it)
       component))

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

(defgeneric component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defun component-parent-pathname (component)
  (aif (component-parent component)
       (component-pathname it)
       *default-pathname-defaults*))

(defmethod component-relative-pathname ((component module))
  (or (slot-value component 'relative-pathname)
      (make-pathname
       :directory `(:relative ,(component-name component))
       :host (pathname-host (component-parent-pathname component)))))

(defmethod component-pathname ((component component))
  (let ((*default-pathname-defaults* (component-parent-pathname component)))
    (merge-pathnames (component-relative-pathname component))))


(defclass system (module)
  ((description :accessor system-description :initarg :description)
   (long-description :accessor long-description :initarg :long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence)))

;;; version-satisfies

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

(defmethod version-satisfies ((c component) version)
  (unless (and version (slot-boundp c 'version))
    (return-from version-satisfies t))
  (let ((x (mapcar #'parse-integer
		   (split (component-version c) nil '(#\.))))
	(y (mapcar #'parse-integer
		   (split version nil '(#\.)))))
    (labels ((bigger (x y)
	       (cond ((not y) t)
		     ((not x) nil)
		     ((> (car x) (car y)) t)
		     ((= (car x) (car y))
		      (bigger (cdr x) (cdr y))))))
      (and (= (car x) (car y))
	   (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding systems

(defvar *defined-systems* (make-hash-table :test 'equal))
(defun coerce-name (name)
   (typecase name
     (component (component-name name))
     (symbol (string-downcase (symbol-name name)))
     (string name)
     (t (sysdef-error "Invalid component designator ~A" name))))

(defvar *central-registry*
  '(*default-pathname-defaults*
    "/home/dan/src/sourceforge/cclan/asdf/systems/"
    #+nil "telent:asdf;systems;"))

(defun system-definition-pathname (system)
  (let ((name (coerce-name system)))
    (dolist (dir *central-registry*)
      (let* ((defaults (if (and (symbolp dir)
				(fboundp dir))
			   (funcall dir name)
			   (eval dir)))
	     (file (and defaults
			(make-pathname
			 :name name :case :local :type "asd"
			 :defaults defaults
			 :version :newest))))
	(if (and file (probe-file file))
	    (return-from system-definition-pathname file))))
    nil))
  

(defun find-system (name &optional (error-p t))
  (let* ((name (coerce-name name))
	 (in-memory (gethash name *defined-systems*))
	 (on-disk (system-definition-pathname name)))	 
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
	  (if error-p (error 'missing-component :requires name))))))

(defun register-system (name system)
  (format t "Registering ~A as ~A ~%" system name)
  (setf (gethash (coerce-name  name) *defined-systems*)
	(cons (get-universal-time) system)))

(defun system-registered-p (name)
  (gethash (coerce-name name) *defined-systems*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding components

(defgeneric find-component (module name &optional version)
  (:documentation "Finds the component with name NAME present in the
MODULE module; if MODULE is nil, then the component is assumed to be a
system."))

(defmethod find-component ((module module) name &optional version)
  (if (slot-boundp module 'components)
      (let ((m (find name (module-components module)
		     :test #'equal :key #'component-name)))
	(if (and m (version-satisfies m version)) m))))
	    

;;; a component with no parent is a system
(defmethod find-component ((module (eql nil)) name &optional version)
  (let ((m (find-system name nil)))
    (if (and m (version-satisfies m version)) m)))

;;; component subclasses

(defclass source-file (component) ())

(defclass cl-source-file (source-file) ())
(defclass c-source-file (source-file) ())
(defclass java-source-file (source-file) ())

(defgeneric source-file-type (component system))
(defmethod source-file-type ((c cl-source-file) (s module)) "lisp")
(defmethod source-file-type ((c c-source-file) (s module)) "c")
(defmethod source-file-type ((c java-source-file) (s module)) "java")
  
(defmethod component-relative-pathname ((component source-file))
  (let ((*default-pathname-defaults* (component-parent-pathname component)))
    (or (slot-value component 'relative-pathname)
	(make-pathname :name (component-name component)
		       :type
		       (source-file-type component
					 (component-system component))))))

(defclass static-file (source-file) ())
(defclass doc-file (static-file) ())
(defclass html-file (doc-file) ())
(defmethod source-file-type ((c static-file) (s module)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operations

;;; one of these is instantiated whenever (operate ) is called

(defclass operation ()
  ((forced-p :initform nil :initarg :force :accessor operation-forced-p )
   (original-initargs :initform nil :initarg :original-initargs
		      :accessor operation-original-initargs)
   (visited-nodes :initform nil :accessor operation-visited-nodes)
   (visiting-nodes :initform nil :accessor operation-visiting-nodes)
   (parent :initform nil :initarg :parent :accessor operation-parent)))

(defmethod shared-initialize :after ((operation operation) slot-names
				     &key force 
				     &allow-other-keys)
  (declare (ignore slot-names force))
  ;; empty method to disable initarg validity checking
  )

(defgeneric perform (operation component))
(defgeneric operation-done-p (operation component))
(defgeneric explain (operation component))
(defgeneric output-files (operation component))

(defun node-for (o c)
  (cons (class-name (class-of o)) c))

(defgeneric operation-ancestor (operation)
  (:documentation   "Recursively chase the operation's parent pointer until we get to the head of the tree"))

(defmethod operation-ancestor ((operation operation))
  (aif (operation-parent operation)
       (operation-ancestor it)
       operation))

(defun make-sub-operation (o type)
  (let ((args (operation-original-initargs o)))
    (apply #'make-instance type :parent o :original-initargs args args)))


(defmethod visit-component ((o operation) (c component))
  (pushnew (node-for o c)
	   (operation-visited-nodes (operation-ancestor o))
	   :test 'equal))

(defmethod component-visited-p ((o operation) (c component))
  (member (node-for o c)
	  (operation-visited-nodes (operation-ancestor o))
	  :test 'equal))

(defmethod (setf visiting-component) (new-value (o operation) (c component))
  (let ((node (node-for o c))
	(a (operation-ancestor o)))
    (if new-value
	(pushnew node (operation-visiting-nodes a) :test 'equal)
	(setf (operation-visiting-nodes a)
	      (remove node  (operation-visiting-nodes a) :test 'equal)))))

(defmethod component-visiting-p ((o operation) (c component))
  (let ((node (cons o c)))
    (member node (operation-visiting-nodes (operation-ancestor o))
	    :test 'equal)))

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
  (cdr (assoc (class-name (class-of o))
	      (slot-value c 'in-order-to))))


;;; So you look at this code and think "why isn't it a bunch of
;;; methods".  And the answer is, because standard method combination
;;; runs :before methods most->least-specific, which is back to front
;;; for our purposes.  And CLISP doesn't have non-standard method
;;; combinations, so let's keep it simple and aspire to portability

;;; we enforce that function is a symbol to allow us to specialize on
;;; (eql 'perform) and (eql 'explain) for :before and :after
(defgeneric traverse (operation component symbol))
(defmethod traverse ((operation operation) (c component) (function
							  symbol))
  (labels ((do-one-dep (required-op required-c required-v)
	     (let ((op (if (subtypep (type-of operation) required-op)
			   operation
			   (make-sub-operation operation required-op)))
		   (dep-c (or (find-component
			       (component-parent c)
			       ;; XXX tacky.  really we should build the
			       ;; in-order-to slot with canonicalized
			       ;; names instead of coercing this late
			       (coerce-name required-c) required-v)
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
		   (missing-dependency (c)
		     (declare (ignore c))
		     (return-from found nil)))
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
		   (destructuring-bind (ignore name version-object) dep
		     (declare (ignore ignore))
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
      (let ((at-least-one nil)
	    (error nil))
	(loop for kid in (module-components c)
	      do (handler-case
		     (traverse operation kid function)		   
		   (missing-dependency (condition)
		     (if (eq (module-if-component-dep-fails c) :fail)
			 (error condition))
		     (setf error condition))
		   (:no-error (c)
		     (declare (ignore c))
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
  (sysdef-error
   "Required method OUTPUT-FILES not implemented for operation ~A, component ~A"
	 (class-of operation) (class-of c)))

;;; compile-op

(defclass compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (on-warnings :initarg :on-warnings :accessor operation-on-warnings
		:initform *compile-file-warnings-behaviour*)
   (on-failure :initarg :on-failure :accessor operation-on-failure
	       :initform *compile-file-failure-behaviour*)))
   

;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((operation compile-op) (c cl-source-file))
  (let ((source-file (component-pathname c)))
    (multiple-value-bind (output warnings-p failure-p)
	(compile-file source-file
		      :output-file (car (output-files operation c)))
      (declare (ignore output))
      (when warnings-p
	(case (operation-on-warnings operation)
	  (:warn (warn "COMPILE-FILE warned while performing ~A on ~A"
		       c operation))
	  (:error (error 'compile-warned :component c :operation operation))
	  (:ignore nil)))
      (when failure-p
	(case (operation-on-failure operation)
	  (:warn (warn "COMPILE-FILE failed while performing ~A on ~A"
		       c operation))
	  (:error (error 'compile-failed :component c :operation operation))
	  (:ignore nil))))))

(defmethod output-files ((operation compile-op) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c))))

(defmethod perform ((operation compile-op) (c static-file))
  nil)

(defmethod output-files ((operation compile-op) (c static-file))
  nil)

;;; load-op

(defclass load-op (operation) ())

(defmethod perform ((o load-op) (c cl-source-file))
  (let ((co (make-sub-operation o 'compile-op)))
    (map nil #'load (output-files co c))))

(defmethod perform ((operation load-op) (c static-file))
  nil)

(defmethod output-files ((operation load-op) (c component))
  nil)

(defmethod component-depends-on ((operation load-op) (c component))
  (cons (list 'compile-op (component-name c))
        (call-next-method)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoking operations

(defun operate (operation-class system &rest args)
  (let ((op (apply #'make-instance operation-class
		   :original-initargs args args))
	(system (if (typep system 'component) system (find-system system))))
    (traverse op system 'perform)))

(defun oos (&rest args)
  "Alias of OPERATE function"
  (apply #'operate args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax

(defmacro defsystem (name &body options)
  (destructuring-bind (&key pathname (class 'system) &allow-other-keys) options
    (let ((component-options
	   (if (member :class options)
	       (remove :class options)
	       options)))
      `(progn
	;; system must be registered before we parse the body, otherwise
	;; we recur when trying to find an existing system of the same name
	;; to reuse options (e.g. pathname) from
	(let ((s (system-registered-p ',name)))
	  (cond ((and s (eq (type-of (cdr s)) ',class))
		 (setf (car s) (get-universal-time)))
		(s
		 #+clisp
		 (sysdef-error "Cannot redefine the existing system ~A with a different class" s)
		 #-clisp
		 (change-class (cdr s) ',class))
		(t
		 (register-system (quote ,name)
				  (make-instance ',class :name ',name)))))
	(parse-component-form nil (apply
				   #'list
				   :module (coerce-name ',name)
				   :pathname
				   (or ,pathname
				       (pathname-sans-name+type *load-truename*)
				       *default-pathname-defaults*)
				   ',component-options))))))
  

(defun class-for-type (parent type)
  (let ((class (find-class
		(or (find-symbol (symbol-name type) *package*)
		    (find-symbol (symbol-name type) #.*package*)) nil)))
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


(defun remove-keys (key-names args)
  (loop for ( name val ) on args by #'cddr
	unless (member (symbol-name name) key-names 
		       :key #'symbol-name :test 'equal)
	append (list name val)))

(defun parse-component-form (parent options)
  (destructuring-bind
	(type name &rest rest &key
	      ;; the following list of keywords is reproduced below in the
	      ;; remove-keys form.  important to keep them in sync
	      components pathname default-component-class
	      perform explain output-files operation-done-p
	      depends-on serialize in-order-to
	      ;; list ends
	      &allow-other-keys) options
    (declare (ignore serialize))
	    ;; XXX add dependencies for serialized subcomponents
	    (let* ((other-args (remove-keys
				'(components pathname default-component-class
				  perform explain output-files operation-done-p
				  depends-on serialize in-order-to)
				rest))
		   (ret
		    (or (find-component parent name)
			(make-instance (class-for-type parent type)))))
	      (apply #'reinitialize-instance
		     ret
		     :name (coerce-name name)
		     :pathname pathname
		     :parent parent
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

#+cmu
(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *trace-output*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format *trace-output* "; $ ~A~%" command)
    (ext:process-exit-code
     (ext:run-program  
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *trace-output*))))

(pushnew :asdf *features*)
