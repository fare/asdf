(in-package :cclan)

;;;; This file contains functions, classes etc that are not part of
;;;; asdf itself, but extend it in various ways useful for maintainers
;;;; of new-style cCLan packages

;;;; This file does not contain references to asdf internals - or
;;;; shouldn't, anyway.  Send bug reports

    
(defun mapappend (function list)
  (let ((f (coerce function 'function)))
    (loop for i in list append (funcall f i))))

(defgeneric list-files (component))
(defmethod list-files ((c source-file))
  (list (component-pathname c)))
(defmethod list-files ((c module))
  (mapappend 'list-files (module-components c)))

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
		   (list-files system)))))
    (= 0
       (run-shell-command "cd ~A && tar cf ~A_~A.tar  ~{~D ~}"
			  (namestring base-path)
			  (component-name system)
			  (component-version system)			  
			  files))))

