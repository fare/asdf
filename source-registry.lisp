;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See the Manual and https://bugs.launchpad.net/asdf/+bug/485918

(asdf/package:define-package :asdf/source-registry
  (:recycle :asdf/source-registry :asdf)
  (:use :common-lisp :asdf/utility :asdf/pathname :asdf/os
        :asdf/upgrade :asdf/find-system :asdf/configuration)
  (:export
   #:*source-registry* #:*source-registry-parameter* #:*default-source-registries*
   #:invalid-source-registry
   #:source-registry #:source-registry-initialized-p
   #:initialize-source-registry #:clear-source-registry #:*source-registry*
   #:disable-source-registry #:ensure-source-registry #:*source-registry-parameter*
   #:*default-source-registry-exclusions* #:*source-registry-exclusions*
   #:*wild-asd* #:directory-asd-files #:register-asd-directory
   #:collect-asds-in-directory #:collect-sub*directories-asd-files
   #:validate-source-registry-directive #:validate-source-registry-form
   #:validate-source-registry-file #:validate-source-registry-directory
   #:parse-source-registry-string #:wrapping-source-registry #:default-source-registry
   #:user-source-registry #:system-source-registry
   #:user-source-registry-directory #:system-source-registry-directory
   #:environment-source-registry #:process-source-registry
   #:compute-source-registry #:flatten-source-registry
   #:sysdef-source-registry-search))
(in-package :asdf/source-registry)

(define-condition invalid-source-registry (invalid-configuration warning)
  ((format :initform (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

;; Using ack 1.2 exclusions
(defvar *default-source-registry-exclusions*
  '(".bzr" ".cdv"
    ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
    ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
    "_sgbak" "autom4te.cache" "cover_db" "_build"
    "debian")) ;; debian often builds stuff under the debian directory... BAD.

(defvar *source-registry-exclusions* *default-source-registry-exclusions*)

(defvar *source-registry* nil
  "Either NIL (for uninitialized), or an equal hash-table, mapping
system names to pathnames of .asd files")

(defun* source-registry-initialized-p ()
  (typep *source-registry* 'hash-table))

(defun* clear-source-registry ()
  "Undoes any initialization of the source registry.
You might want to call that before you dump an image that would be resumed
with a different configuration, so the configuration would be re-read then."
  (setf *source-registry* nil)
  (values))

(defparameter *wild-asd*
  (make-pathname* :directory nil :name *wild* :type "asd" :version :newest))

(defun* directory-asd-files (directory)
  (directory-files directory *wild-asd*))

(defun* collect-asds-in-directory (directory collect)
  (map () collect (directory-asd-files directory)))

(defun* collect-sub*directories-asd-files
    (directory &key (exclude *default-source-registry-exclusions*) collect)
  (collect-sub*directories
   directory
   (constantly t)
   #'(lambda (x) (not (member (car (last (pathname-directory x))) exclude :test #'equal)))
   #'(lambda (dir) (collect-asds-in-directory dir collect))))

(defun* validate-source-registry-directive (directive)
  (or (member directive '(:default-registry))
      (and (consp directive)
           (let ((rest (rest directive)))
             (case (first directive)
               ((:include :directory :tree)
                (and (length=n-p rest 1)
                     (location-designator-p (first rest))))
               ((:exclude :also-exclude)
                (every #'stringp rest))
               ((:default-registry)
                (null rest)))))))

(defun* validate-source-registry-form (form &key location)
  (validate-configuration-form
   form :source-registry 'validate-source-registry-directive
   :location location :invalid-form-reporter 'invalid-source-registry))

(defun* validate-source-registry-file (file)
  (validate-configuration-file
   file 'validate-source-registry-form :description "a source registry"))

(defun* validate-source-registry-directory (directory)
  (validate-configuration-directory
   directory :source-registry 'validate-source-registry-directive
   :invalid-form-reporter 'invalid-source-registry))

(defun* parse-source-registry-string (string &key location)
  (cond
    ((or (null string) (equal string ""))
     '(:source-registry :inherit-configuration))
    ((not (stringp string))
     (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
    ((find (char string 0) "\"(")
     (validate-source-registry-form (read-from-string string) :location location))
    (t
     (loop
      :with inherit = nil
      :with directives = ()
      :with start = 0
      :with end = (length string)
      :with separator = (inter-directory-separator)
      :for pos = (position separator string :start start) :do
      (let ((s (subseq string start (or pos end))))
        (flet ((check (dir)
                 (unless (absolute-pathname-p dir)
                   (error (compatfmt "~@<source-registry string must specify absolute pathnames: ~3i~_~S~@:>") string))
                 dir))
          (cond
            ((equal "" s) ; empty element: inherit
             (when inherit
               (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                      string))
             (setf inherit t)
             (push ':inherit-configuration directives))
            ((string-suffix-p s "//") ;; TODO: allow for doubling of separator even outside Unix?
             (push `(:tree ,(check (subseq s 0 (- (length s) 2)))) directives))
            (t
             (push `(:directory ,(check s)) directives))))
        (cond
          (pos
           (setf start (1+ pos)))
          (t
           (unless inherit
             (push '(:ignore-inherited-configuration) directives))
           (return `(:source-registry ,@(nreverse directives))))))))))

(defun* register-asd-directory (directory &key recurse exclude collect)
  (if (not recurse)
      (collect-asds-in-directory directory collect)
      (collect-sub*directories-asd-files
       directory :exclude exclude :collect collect)))

(defparameter *default-source-registries*
  '(environment-source-registry
    user-source-registry
    user-source-registry-directory
    system-source-registry
    system-source-registry-directory
    default-source-registry))

(defparameter *source-registry-file* (coerce-pathname "source-registry.conf"))
(defparameter *source-registry-directory* (coerce-pathname "source-registry.conf.d/"))

(defun* wrapping-source-registry ()
  `(:source-registry
    #+(or ecl sbcl) (:tree ,(lisp-implementation-directory :truename t))
    #+mkcl (:tree ,(translate-logical-pathname "CONTRIB:"))
    :inherit-configuration
    #+cmu (:tree #p"modules:")
    #+scl (:tree #p"file://modules/")))
(defun* default-source-registry ()
  `(:source-registry
    #+sbcl (:directory ,(subpathname (user-homedir) ".sbcl/systems/"))
    ,@(loop :for dir :in
        `(,@(when (os-unix-p)
              `(,(or (getenv-absolute-directory "XDG_DATA_HOME")
                     (subpathname (user-homedir) ".local/share/"))
                ,@(or (getenv-absolute-directories "XDG_DATA_DIRS")
                      '("/usr/local/share" "/usr/share"))))
          ,@(when (os-windows-p)
              (mapcar 'get-folder-path '(:local-appdata :appdata :common-appdata))))
        :collect `(:directory ,(subpathname* dir "common-lisp/systems/"))
        :collect `(:tree ,(subpathname* dir "common-lisp/source/")))
    :inherit-configuration))
(defun* user-source-registry (&key (direction :input))
  (in-user-configuration-directory *source-registry-file* :direction direction))
(defun* system-source-registry (&key (direction :input))
  (in-system-configuration-directory *source-registry-file* :direction direction))
(defun* user-source-registry-directory (&key (direction :input))
  (in-user-configuration-directory *source-registry-directory* :direction direction))
(defun* system-source-registry-directory (&key (direction :input))
  (in-system-configuration-directory *source-registry-directory* :direction direction))
(defun* environment-source-registry ()
  (getenv "CL_SOURCE_REGISTRY"))

(defgeneric* process-source-registry (spec &key inherit register))

(defun* inherit-source-registry (inherit &key register)
  (when inherit
    (process-source-registry (first inherit) :register register :inherit (rest inherit))))

(defun* process-source-registry-directive (directive &key inherit register)
  (destructuring-bind (kw &rest rest) (if (consp directive) directive (list directive))
    (ecase kw
      ((:include)
       (destructuring-bind (pathname) rest
         (process-source-registry (resolve-location pathname) :inherit nil :register register)))
      ((:directory)
       (destructuring-bind (pathname) rest
         (when pathname
           (funcall register (resolve-location pathname :directory t)))))
      ((:tree)
       (destructuring-bind (pathname) rest
         (when pathname
           (funcall register (resolve-location pathname :directory t)
                    :recurse t :exclude *source-registry-exclusions*))))
      ((:exclude)
       (setf *source-registry-exclusions* rest))
      ((:also-exclude)
       (appendf *source-registry-exclusions* rest))
      ((:default-registry)
       (inherit-source-registry '(default-source-registry) :register register))
      ((:inherit-configuration)
       (inherit-source-registry inherit :register register))
      ((:ignore-inherited-configuration)
       nil)))
  nil)

(defmethod process-source-registry ((x symbol) &key inherit register)
  (process-source-registry (funcall x) :inherit inherit :register register))
(defmethod process-source-registry ((pathname #-gcl<2.7 pathname #+gcl<2.7 t) &key inherit register)
  (cond
    ((directory-pathname-p pathname)
     (let ((*here-directory* (truenamize pathname)))
       (process-source-registry (validate-source-registry-directory pathname)
                                :inherit inherit :register register)))
    ((probe-file* pathname)
     (let ((*here-directory* (pathname-directory-pathname pathname)))
       (process-source-registry (validate-source-registry-file pathname)
                                :inherit inherit :register register)))
    (t
     (inherit-source-registry inherit :register register))))
(defmethod process-source-registry ((string string) &key inherit register)
  (process-source-registry (parse-source-registry-string string)
                           :inherit inherit :register register))
(defmethod process-source-registry ((x null) &key inherit register)
  (declare (ignorable x))
  (inherit-source-registry inherit :register register))
(defmethod process-source-registry ((form cons) &key inherit register)
  (let ((*source-registry-exclusions* *default-source-registry-exclusions*))
    (dolist (directive (cdr (validate-source-registry-form form)))
      (process-source-registry-directive directive :inherit inherit :register register))))

(defun* flatten-source-registry (&optional parameter)
  (remove-duplicates
   (while-collecting (collect)
     (with-pathname-defaults () ;; be location-independent
       (inherit-source-registry
        `(wrapping-source-registry
          ,parameter
          ,@*default-source-registries*)
        :register #'(lambda (directory &key recurse exclude)
                      (collect (list directory :recurse recurse :exclude exclude))))))
   :test 'equal :from-end t))

;; Will read the configuration and initialize all internal variables.
(defun* compute-source-registry (&optional parameter (registry *source-registry*))
  (dolist (entry (flatten-source-registry parameter))
    (destructuring-bind (directory &key recurse exclude) entry
      (let* ((h (make-hash-table :test 'equal))) ; table to detect duplicates
        (register-asd-directory
         directory :recurse recurse :exclude exclude :collect
         #'(lambda (asd)
             (let* ((name (pathname-name asd))
                    (name (if (typep asd 'logical-pathname)
                              ;; logical pathnames are upper-case,
                              ;; at least in the CLHS and on SBCL,
                              ;; yet (coerce-name :foo) is lower-case.
                              ;; won't work well with (load-system "Foo")
                              ;; instead of (load-system 'foo)
                              (string-downcase name)
                              name)))
               (cond
                 ((gethash name registry) ; already shadowed by something else
                  nil)
                 ((gethash name h) ; conflict at current level
                  (when *asdf-verbose*
                    (warn (compatfmt "~@<In source-registry entry ~A~@[/~*~] ~
                                found several entries for ~A - picking ~S over ~S~:>")
                          directory recurse name (gethash name h) asd)))
                 (t
                  (setf (gethash name registry) asd)
                  (setf (gethash name h) asd))))))
        h)))
  (values))

(defvar *source-registry-parameter* nil)

(defun* initialize-source-registry (&optional (parameter *source-registry-parameter*))
  #-clisp ;; CLISP really hates our package munging. Don't try to load it twice.
  (setf *asdf-upgrade-already-attempted* nil) ;; in case a new ASDF appears in the registry
  (setf *source-registry-parameter* parameter)
  (setf *source-registry* (make-hash-table :test 'equal))
  (compute-source-registry parameter))

;; Checks an initial variable to see whether the state is initialized
;; or cleared. In the former case, return current configuration; in
;; the latter, initialize.  ASDF will call this function at the start
;; of (asdf:find-system) to make sure the source registry is initialized.
;; However, it will do so *without* a parameter, at which point it
;; will be too late to provide a parameter to this function, though
;; you may override the configuration explicitly by calling
;; initialize-source-registry directly with your parameter.
(defun* ensure-source-registry (&optional parameter)
  (unless (source-registry-initialized-p)
    (initialize-source-registry parameter))
  (values))

(defun* sysdef-source-registry-search (system)
  (ensure-source-registry)
  (values (gethash (coerce-name system) *source-registry*)))

(pushnew 'clear-source-registry *clear-configuration-hook*)
