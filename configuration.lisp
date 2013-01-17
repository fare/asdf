;;;; ---------------------------------------------------------------------------
;;;; Generic support for configuration files

(asdf/package:define-package :asdf/configuration
  (:recycle :asdf/configuration :asdf)
  (:use :common-lisp :asdf/compatibility :asdf/utility
   :asdf/pathname :asdf/stream :asdf/os :asdf/image)
  (:export
   #:get-folder-path
   #:user-configuration-directories #:system-configuration-directories
   #:in-first-directory
   #:in-user-configuration-directory #:in-system-configuration-directory
   #:validate-configuration-form #:validate-configuration-file #:validate-configuration-directory
   #:configuration-inheritance-directive-p
   #:report-invalid-form #:invalid-configuration #:*ignored-configuration-form*
   #:*clear-configuration-hook* #:clear-configuration #:register-clear-configuration-hook
   #:resolve-location #:location-designator-p #:location-function-p #:*here-directory*
   #:resolve-relative-location-component #:resolve-absolute-location-component))
(in-package :asdf/configuration)

(define-condition invalid-configuration ()
  ((form :reader condition-form :initarg :form)
   (location :reader condition-location :initarg :location)
   (format :reader condition-format :initarg :format)
   (arguments :reader condition-arguments :initarg :arguments :initform nil))
  (:report (lambda (c s)
               (format s (compatfmt "~@<~? (will be skipped)~@:>")
                       (condition-format c)
                       (list* (condition-form c) (condition-location c)
                              (condition-arguments c))))))

(defun* get-folder-path (folder)
  (or ;; this semi-portably implements a subset of the functionality of lispworks' sys:get-folder-path
   #+(and lispworks mswindows) (sys:get-folder-path folder)
   ;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\AppData
   (ecase folder
    (:local-appdata (getenv-absolute-directory "LOCALAPPDATA"))
    (:appdata (getenv-absolute-directory "APPDATA"))
    (:common-appdata (or (getenv-absolute-directory "ALLUSERSAPPDATA")
                         (subpathname* (getenv-absolute-directory "ALLUSERSPROFILE") "Application Data/"))))))

(defun* user-configuration-directories ()
  (let ((dirs
         `(,@(when (os-unix-p)
               (cons
                (subpathname* (getenv-absolute-directory "XDG_CONFIG_HOME") "common-lisp/")
                (loop :for dir :in (getenv-absolute-directories "XDG_CONFIG_DIRS")
                  :collect (subpathname* dir "common-lisp/"))))
           ,@(when (os-windows-p)
               `(,(subpathname* (get-folder-path :local-appdata) "common-lisp/config/")
                 ,(subpathname* (get-folder-path :appdata) "common-lisp/config/")))
           ,(subpathname (user-homedir) ".config/common-lisp/"))))
    (remove-duplicates (remove-if-not #'absolute-pathname-p dirs)
                       :from-end t :test 'equal)))

(defun* system-configuration-directories ()
  (cond
    ((os-unix-p) '(#p"/etc/common-lisp/"))
    ((os-windows-p)
     (if-bind (it (subpathname* (get-folder-path :common-appdata) "common-lisp/config/"))
       (list it)))))

(defun* in-first-directory (dirs x &key (direction :input))
  (loop :with fun = (ecase direction
                      ((nil :input :probe) 'probe-file*)
                      ((:output :io) 'identity))
    :for dir :in dirs
    :thereis (and dir (funcall fun (merge-pathnames* x (ensure-directory-pathname dir))))))

(defun* in-user-configuration-directory (x &key (direction :input))
  (in-first-directory (user-configuration-directories) x :direction direction))
(defun* in-system-configuration-directory (x &key (direction :input))
  (in-first-directory (system-configuration-directories) x :direction direction))

(defun* configuration-inheritance-directive-p (x)
  (let ((kw '(:inherit-configuration :ignore-inherited-configuration)))
    (or (member x kw)
        (and (length=n-p x 1) (member (car x) kw)))))

(defun* report-invalid-form (reporter &rest args)
  (etypecase reporter
    (null
     (apply 'error 'invalid-configuration args))
    (function
     (apply reporter args))
    ((or symbol string)
     (apply 'error reporter args))
    (cons
     (apply 'apply (append reporter args)))))

(defvar *ignored-configuration-form* nil)

(defun* validate-configuration-form (form tag directive-validator
                                    &key location invalid-form-reporter)
  (unless (and (consp form) (eq (car form) tag))
    (setf *ignored-configuration-form* t)
    (report-invalid-form invalid-form-reporter :form form :location location)
    (return-from validate-configuration-form nil))
  (loop :with inherit = 0 :with ignore-invalid-p = nil :with x = (list tag)
    :for directive :in (cdr form)
    :when (cond
            ((configuration-inheritance-directive-p directive)
             (incf inherit) t)
            ((eq directive :ignore-invalid-entries)
             (setf ignore-invalid-p t) t)
            ((funcall directive-validator directive)
             t)
            (ignore-invalid-p
             nil)
            (t
             (setf *ignored-configuration-form* t)
             (report-invalid-form invalid-form-reporter :form directive :location location)
             nil))
    :do (push directive x)
    :finally
    (unless (= inherit 1)
      (report-invalid-form invalid-form-reporter
             :arguments (list (compatfmt "~@<One and only one of ~S or ~S is required.~@:>")
                              :inherit-configuration :ignore-inherited-configuration)))
    (return (nreverse x))))

(defun* validate-configuration-file (file validator &key description)
  (let ((forms (read-file-forms file)))
    (unless (length=n-p forms 1)
      (error (compatfmt "~@<One and only one form allowed for ~A. Got: ~3i~_~S~@:>~%")
             description forms))
    (funcall validator (car forms) :location file)))

(defun* validate-configuration-directory (directory tag validator &key invalid-form-reporter)
  "Map the VALIDATOR across the .conf files in DIRECTORY, the TAG will
be applied to the results to yield a configuration form.  Current
values of TAG include :source-registry and :output-translations."
  (let ((files (sort (ignore-errors
                       (remove-if
                        'hidden-pathname-p
                        (directory* (make-pathname :name *wild* :type "conf" :defaults directory))))
                     #'string< :key #'namestring)))
    `(,tag
      ,@(loop :for file :in files :append
          (loop :with ignore-invalid-p = nil
            :for form :in (read-file-forms file)
            :when (eq form :ignore-invalid-entries)
              :do (setf ignore-invalid-p t)
            :else
              :when (funcall validator form)
                :collect form
              :else
                :when ignore-invalid-p
                  :do (setf *ignored-configuration-form* t)
                :else
                  :do (report-invalid-form invalid-form-reporter :form form :location file)))
      :inherit-configuration)))

(defun* resolve-relative-location-component (x &key want-directory wilden)
  (ensure-pathname
   (etypecase x
     (pathname x)
     (string (parse-unix-namestring
              x :want-directory want-directory))
     (cons
      (if (null (cdr x))
          (resolve-relative-location-component
           (car x) :want-directory want-directory :wilden wilden)
          (let* ((car (resolve-relative-location-component
                       (car x) :want-directory t :wilden nil)))
            (merge-pathnames*
             (resolve-relative-location-component
              (cdr x) :want-directory want-directory :wilden wilden)
             car))))
     ((eql :*/) *wild-directory*)
     ((eql :**/) *wild-inferiors*)
     ((eql :*.*.*) *wild-file*)
     ((eql :implementation)
      (parse-unix-namestring
       (implementation-identifier) :want-directory t))
     ((eql :implementation-type)
      (parse-unix-namestring
       (string-downcase (implementation-type)) :want-directory t))
     ((eql :hostname)
      (parse-unix-namestring (hostname) :want-directory t)))
  :wilden (and wilden (not (pathnamep x)) (not (member x '(:*/ :**/ :*.*.*))))
  :want-relative t))

(defvar *here-directory* nil
  "This special variable is bound to the currect directory during calls to
PROCESS-SOURCE-REGISTRY in order that we be able to interpret the :here
directive.")

(defvar *user-cache* nil
  "A specification as per RESOLVE-LOCATION of where the user keeps his FASL cache")

(defun compute-user-cache ()
  (setf *user-cache*
        (flet ((try (x &rest sub) (and x `(,x ,@sub))))
          (or
           (try (getenv-absolute-directory "XDG_CACHE_HOME") "common-lisp" :implementation)
           (when (os-windows-p)
             (try (or (get-folder-path :local-appdata)
                      (get-folder-path :appdata))
                  "common-lisp" "cache" :implementation))
           '(:home ".cache" "common-lisp" :implementation)))))
(register-image-restore-hook 'compute-user-cache)

(defun* resolve-absolute-location-component (x &key want-directory wilden)
  (ensure-pathname
   (etypecase x
     (pathname x)
     (string
      (let ((p #-mcl (parse-namestring x)
               #+mcl (probe-posix x)))
        #+mcl (unless p (error "POSIX pathname ~S does not exist" x))
        (if want-directory (ensure-directory-pathname p) p)))
     (cons
      (return-from resolve-absolute-location-component
        (if (null (cdr x))
            (resolve-absolute-location-component
             (car x) :want-directory want-directory :wilden wilden)
            (merge-pathnames*
             (resolve-relative-location-component
              (cdr x) :want-directory want-directory :wilden wilden)
             (resolve-absolute-location-component
              (car x) :want-directory t :wilden nil)))))
     ((eql :root)
      ;; special magic! we return a relative pathname,
      ;; but what it means to the output-translations is
      ;; "relative to the root of the source pathname's host and device".
      (return-from resolve-absolute-location-component
        (let ((p (make-pathname* :directory '(:relative))))
          (if wilden (wilden p) p))))
     ((eql :home) (user-homedir))
     ((eql :here) (resolve-absolute-location-component
                   *here-directory* :want-directory t :wilden nil))
     ((eql :user-cache) (resolve-absolute-location-component
                         *user-cache* :want-directory t :wilden nil)))
   :wilden (and wilden (not (pathnamep x)))
   :want-absolute t))

(defun* resolve-location (x &key want-directory wilden directory)
  (when directory (setf want-directory t)) ;; :directory backward compatibility, until 2014-01-16.
  (if (atom x)
      (resolve-absolute-location-component x :want-directory want-directory :wilden wilden)
      (loop :with path = (resolve-absolute-location-component
                          (car x) :want-directory (and (or want-directory (cdr x)) t)
                          :wilden (and wilden (null (cdr x))))
        :for (component . morep) :on (cdr x)
        :for dir = (and (or morep want-directory) t)
        :for wild = (and wilden (not morep))
        :do (setf path (merge-pathnames*
                        (resolve-relative-location-component
                         component :want-directory dir :wilden wild)
                        path))
        :finally (return path))))

(defun* location-designator-p (x)
  (flet ((absolute-component-p (c)
           (typep c '(or string pathname
                      (member :root :home :here :user-cache))))
         (relative-component-p (c)
           (typep c '(or string pathname
                      (member :*/ :**/ :*.*.* :implementation :implementation-type)))))
    (or (typep x 'boolean)
        (absolute-component-p x)
        (and (consp x) (absolute-component-p (first x)) (every #'relative-component-p (rest x))))))

(defun* location-function-p (x)
  (and
   (length=n-p x 2)
   (eq (car x) :function)
   (or (symbolp (cadr x))
       (and (consp (cadr x))
            (eq (caadr x) 'lambda)
            (length=n-p (cadadr x) 2)))))

(defvar *clear-configuration-hook* '())

(defun* register-clear-configuration-hook (hook-function &optional call-now-p)
  (register-hook-function '*clear-configuration-hook* hook-function call-now-p))

(defun* clear-configuration ()
  (call-functions *clear-configuration-hook*))

(register-image-dump-hook 'clear-configuration)
