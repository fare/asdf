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
   (depends-on :initform nil
	       :accessor component-depends-on :initarg :depends-on)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied

   ;; :pathname is actually a realy bad choice of name for this slot,
   ;; because quite often that's not what we actually use it for
   (pathname :initarg :pathname)))

(defmethod component-pathname  ((component component))
  (let ((*default-pathname-defaults* *component-parent-pathname*))
    (if (slot-boundp component 'pathname)
	(let ((p (slot-value component 'pathname)))
	  (cond ((pathnamep p)
		 (merge-pathnames p))
		((and (stringp p) (> (length p) 0))
		 (destructuring-bind (name type) (split p 2 '(#\.))
		   (merge-pathnames (make-pathname :name name :type type))))
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
  (make-pathname :name (component-name component)
		 :type (car (rassoc (class-name (class-of component))
				    *known-extensions* ))))

(defclass c-source-file (source-file) ())
(defclass java-source-file (source-file) ())

;;; XXX we don't need the first of these if we have the second, probably
(defvar *component-parent-pathname*)
(defvar *component-parent*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operations

;;; one of these is instantiated whenever (operate ) is called

(defclass operation ()
  ((forced-p :initform nil :initarg :force :accessor operation-forced-p )
   (visited-components :initform nil :accessor operation-visited-components)
   (visiting-components :initform nil :accessor operation-visiting-components)))

(defgeneric perform (operation system))
(defgeneric operation-done-p (operation system))
(defgeneric explain (operation system))
(defgeneric output-files (operation system))

(defmethod visit-component ((o operation) (c component))
  (pushnew c (operation-visited-components o)))

(defmethod component-visited-p ((o operation) (c component))
  (member c (operation-visited-components o)))

(defmethod (setf visiting-component) (new-value (o operation) (c component))
  (if new-value
      (pushnew c (operation-visiting-components o))
      (setf (operation-visiting-components o)
	    (remove c (operation-visiting-components o)))))

(defmethod component-visiting-p ((o operation) (c component))
  (member c (operation-visiting-components o)))

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

;;; So you look at this code and think "why isn't it a bunch of
;;; methods".  And the answer is, because standard method combination
;;; runs :before methods most->least-specific, which is back to front
;;; for our purposes.  And CLISP apparently doesn't have non-standard
;;; method combinations, so let's keep it simple and aspire to
;;; portability

(defmethod traverse ((operation operation) (c component) function)
  ; dependencies
  (if (and nil (component-visiting-p operation c))
      (error 'circular-dependency :components (list c)))
  (setf (visiting-component operation c) t)
  (mapc (lambda (k)
	  (traverse operation (find-component *component-parent* k)
		    function))
	(component-depends-on c))
  ;; constituent bits
  (when (typep c 'module)
    (let ((*component-parent-pathname*
	   (component-pathname c))
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
	(error 'compile-failed :component c :operation operation))
      ;; now load the file.  this is a hack which will go away when we
      ;; work out dependencies properly
      (load output))))

(defmethod output-files ((operation compile-system) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c))))

;;; load-system

(defclass load-system (operation) ())

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
	(*component-parent* nil)
	(system (if (typep system 'component) system (find-system system))))
    (traverse op system 'perform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding systems

(defvar *defined-systems* (make-hash-table :test 'equal))
(defparameter *central-registry*
  '(*default-pathname-defaults*
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
  (let* ((name (if (symbolp name) (symbol-name name) name))
	 (args `(:name ,name (process-option-list m (quote ,options)))))
    ;; this macro is called (sic) during executing of find-system, so
    ;; we had better not call find-system recursively
    `(let ((m (or (cdr (gethash ,name *defined-systems*))
		  (make-instance 'module :name ,name  ))))
      (setf (gethash ,name *defined-systems*) (cons 0 m))
      (apply #'reinitialize-instance m  ,@args))))

(defmethod process-option-list ((c component) options)
  (loop for (name value) on options by #'cddr
	append (process-option c name value)))

(defmethod process-option ((c component) option  value)
  (list option value))


#|
source-file components defined with (:file "a-string") or "a-string"
will have the string parsed into name and type as if it were a
filename, and an instance of the appropriate source-file subclass
created.  If a type is not provided, it will default to the parent's
default constituent type.
|#

(defun create-instance-for-component (parent-component keyword name args)
  (let* ((name-bits (split name 2 '(#\.)))
	 (name (car name-bits))
	 (extension (second name-bits))
	 (class
	  (cond ((eq keyword :file)
		 (if extension
		     (cdr (assoc extension *known-extensions* :test 'equal))
		     (module-default-component-class parent-component)))
		((eq keyword :module) 'module)
		(t (intern (symbol-name keyword) *package*)))))
    (let ((instance (or (find-component parent-component name)
			(make-instance class :name name))))
      (apply #'reinitialize-instance
	     instance
	     :name name (process-option-list instance args))
      instance)))

(defmethod process-option ((c component) (option (eql :components)) value)
  (list :components
	(mapcar
	 (lambda (i)
	   (if (consp i)
	       (create-instance-for-component c (first i) (second i) (cddr i))
	       (create-instance-for-component c :file (second i) nil)))
	 value)))

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

;;; mk-compatibility
(defmethod process-option ((c component) (option (eql :source-pathname)) value)
  (list :pathname value))