(in-package :cclan)

;;;; This file contains functions, classes etc that are not part of
;;;; asdf itself, but extend it in various ways useful for maintainers
;;;; of new-style cCLan packages

;;;; The public interface consists of the functions whose symbols are 
;;;; exported from the package

;;;; This file does not contain references to asdf internals - or
;;;; shouldn't, anyway.  Send bug reports

    
(defun mapappend (function list)
  (let ((f (coerce function 'function)))
    (loop for i in list append (funcall f i))))

(defgeneric all-components (component))
(defmethod all-components ((source-file source-file))
  (list source-file))

(defmethod all-components ((module module))
  (cons module (mapappend #'all-components (module-components module))))

(defmethod all-components ((module symbol))
  (all-components (find-system module)))

(defun cvs-tag (system)
  (let* ((system (find-system system))
	 (directory (component-pathname system))
	 (version (component-version system)))
    (run-shell-command "cd ~A && cvs tag -F cclan_version_~A"
		       (namestring directory)
		       (substitute #\_ #\. version))))

(defun make-tar-file (system)
  "Make a tar file named SYSTEM-NAME_VERSION.tar containing all the files in SYSTEM.  The file is created in the directory containing the system directory.  Returns T on success"
  (let* ((system (find-system system))
	 (sys-path  (component-pathname system))
	 (base-path (make-pathname
		     :directory (butlast (pathname-directory sys-path))
		     :defaults sys-path))
	 (files
	  (mapcar (lambda (x) (enough-namestring x base-path))
		  (cons
		   (truename (system-definition-pathname system))
		   (mapcar #'component-pathname (all-components system))))))
    (= 0
       (run-shell-command "cd ~A && tar cf ~A_~A.tar  ~{~D ~}"
			  (namestring base-path)
			  (component-name system)
			  (component-version system)			  
			  (remove-if-not #'pathname-name files)))))

