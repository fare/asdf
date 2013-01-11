;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp pathnames

(asdf/package:define-package :asdf/pathname
  (:recycle :asdf/pathname :asdf)
  #+gcl<2.7 (:shadowing-import-from :system :*load-pathname*) ;; GCL 2.6 sucks
  (:use :common-lisp :asdf/compatibility :asdf/utility)
  (:export
   #:*resolve-symlinks*
   ;; Making and merging pathnames, portably
   #:normalize-pathname-directory-component #:denormalize-pathname-directory-component
   #:merge-pathname-directory-components #:make-pathname*
   #:make-pathname-component-logical #:make-pathname-logical
   #:merge-pathnames*
   ;; Directories
   #:pathname-directory-pathname #:pathname-parent-directory-pathname
   #:directory-pathname-p #:ensure-directory-pathname
   ;; Absolute vs relative pathnames
   #:absolute-pathname-p #:ensure-pathname-absolute
   #:relativize-directory-component #:relativize-pathname-directory
   ;; Parsing filenames and lists thereof
   #:component-name-to-pathname-components
   #:split-name-type #:coerce-pathname #:subpathname #:subpathname* #:ensure-pathname*
   #:hidden-file-p
   ;; Resolving symlinks somewhat
   #:truenamize #:resolve-symlinks #:resolve-symlinks*
   ;; Wildcard pathnames
   #:*wild* #:*wild-file* #:*wild-directory* #:*wild-inferiors* #:*wild-path* #:wilden
   ;; Pathname host and its root
   #:pathname-root #:directory-separator-for-host
   #:directorize-pathname-host-device
   ;; defaults
   #:nil-pathname #:with-pathname-defaults
   ;; probe filesystem
   #:truename* #:probe-file* #:safe-file-write-date
   #:subdirectories #:directory-files #:directory*
   #:filter-logical-directory-results #:collect-sub*directories
   ;; Simple filesystem operations
   #:ensure-all-directories-exist
   #:rename-file-overwriting-target
   #:delete-file-if-exists
   ;; Translate a pathname
   #:translate-pathname*
   #:native-namestring #:parse-native-namestring
   ;; temporary 
   #:add-pathname-suffix #:tmpize-pathname
   #:call-with-staging-pathname #:with-staging-pathname
   ;; basic pathnames
   #:load-pathname #:default-directory #:root-pathname
   ;; physical pathnames
   #:physical-pathname-p #:sane-physical-pathname
   ;; Windows shortcut support
   #:read-null-terminated-string #:read-little-endian
   #:parse-file-location-info #:parse-windows-shortcut))
(in-package :asdf/pathname)

;;; User-visible parameters
(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.

Defaults to T.")

;;; The hell of portably making and merging pathnames!

(defun* normalize-pathname-directory-component (directory)
  "Given a pathname directory component, return an equivalent form that is a list"
  #+gcl<2.7 (setf directory (substitute :back :parent directory))
  (cond
    #-(or cmu sbcl scl) ;; these implementations already normalize directory components.
    ((stringp directory) `(:absolute ,directory))
    #+gcl<2.7
    ((and (consp directory) (eq :root (first directory)))
     `(:absolute ,@(rest directory)))
    ((or (null directory)
         (and (consp directory) (member (first directory) '(:absolute :relative))))
     directory)
    #+gcl<2.7
    ((consp directory)
     `(:relative ,@directory))
    (t
     (error (compatfmt "~@<Unrecognized pathname directory component ~S~@:>") directory))))

(defun* denormalize-pathname-directory-component (directory-component)
  #-gcl<2.7 directory-component
  #+gcl<2.7
  (let ((d (substitute-if :parent (lambda (x) (member x '(:up :back)))
                          directory-component)))
    (cond
      ((and (consp d) (eq :relative (first d))) (rest d))
      ((and (consp d) (eq :absolute (first d))) `(:root ,@(rest d)))
      (t d))))

(defun* merge-pathname-directory-components (specified defaults)
  ;; Helper for merge-pathnames* that handles directory components.
  (let ((directory (normalize-pathname-directory-component specified)))
    (ecase (first directory)
      ((nil) defaults)
      (:absolute specified)
      (:relative
       (let ((defdir (normalize-pathname-directory-component defaults))
             (reldir (cdr directory)))
         (cond
           ((null defdir)
            directory)
           ((not (eq :back (first reldir)))
            (append defdir reldir))
           (t
            (loop :with defabs = (first defdir)
              :with defrev = (reverse (rest defdir))
              :while (and (eq :back (car reldir))
                          (or (and (eq :absolute defabs) (null defrev))
                              (stringp (car defrev))))
              :do (pop reldir) (pop defrev)
              :finally (return (cons defabs (append (reverse defrev) reldir)))))))))))

(defun* make-pathname* (&rest keys &key (directory nil directoryp)
                              host device name type version defaults #+scl &allow-other-keys)
  (declare (ignore host device name type version defaults))
  (apply 'make-pathname
         (append (when directoryp
                   `(:directory ,(denormalize-pathname-directory-component directory)))
                 keys)))

(defun* make-pathname-component-logical (x)
  "Make a pathname component suitable for use in a logical-pathname"
  (typecase x
    ((eql :unspecific) nil)
    #+clisp (string (string-upcase x))
    #+clisp (cons (mapcar 'make-pathname-component-logical x))
    (t x)))

(defun* make-pathname-logical (pathname host)
  "Take a PATHNAME's directory, name, type and version components,
and make a new pathname with corresponding components and specified logical HOST"
  (make-pathname*
   :host host
   :directory (make-pathname-component-logical (pathname-directory pathname))
   :name (make-pathname-component-logical (pathname-name pathname))
   :type (make-pathname-component-logical (pathname-type pathname))
   :version (make-pathname-component-logical (pathname-version pathname))))

(defun* merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
  "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that
if the SPECIFIED pathname does not have an absolute directory,
then the HOST and DEVICE both come from the DEFAULTS, whereas
if the SPECIFIED pathname does have an absolute directory,
then the HOST and DEVICE both come from the SPECIFIED.
Also, if either argument is NIL, then the other argument is returned unmodified."
  (when (null specified) (return-from merge-pathnames* defaults))
  (when (null defaults) (return-from merge-pathnames* specified))
  #+scl
  (ext:resolve-pathname specified defaults)
  #-scl
  (let* ((specified (pathname specified))
         (defaults (pathname defaults))
         (directory (normalize-pathname-directory-component (pathname-directory specified)))
         (name (or (pathname-name specified) (pathname-name defaults)))
         (type (or (pathname-type specified) (pathname-type defaults)))
         (version (or (pathname-version specified) (pathname-version defaults))))
    (labels ((unspecific-handler (p)
               (if (typep p 'logical-pathname) #'make-pathname-component-logical #'identity)))
      (multiple-value-bind (host device directory unspecific-handler)
          (ecase (first directory)
            ((:absolute)
             (values (pathname-host specified)
                     (pathname-device specified)
                     directory
                     (unspecific-handler specified)))
            ((nil :relative)
             (values (pathname-host defaults)
                     (pathname-device defaults)
                     (merge-pathname-directory-components directory (pathname-directory defaults))
                     (unspecific-handler defaults))))
        (make-pathname* :host host :device device :directory directory
                        :name (funcall unspecific-handler name)
                        :type (funcall unspecific-handler type)
                        :version (funcall unspecific-handler version))))))


;;; Directories
(defun* pathname-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname :name nil :type nil :version nil :defaults pathname)))

(defun* pathname-parent-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname* :name nil :type nil :version nil
                    :directory (merge-pathname-directory-components
                                '(:relative :back) (pathname-directory pathname))
                    :defaults pathname)))

(defun* directory-pathname-p (pathname)
  "Does PATHNAME represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be NIL,
:UNSPECIFIC or the empty string.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing directory."
  (when pathname
    (let ((pathname (pathname pathname)))
      (flet ((check-one (x)
               (member x '(nil :unspecific "") :test 'equal)))
        (and (not (wild-pathname-p pathname))
             (check-one (pathname-name pathname))
             (check-one (pathname-type pathname))
             t)))))

(defun* ensure-directory-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (cond
   ((stringp pathspec)
    (ensure-directory-pathname (pathname pathspec)))
   ((not (pathnamep pathspec))
    (error (compatfmt "~@<Invalid pathname designator ~S~@:>") pathspec))
   ((wild-pathname-p pathspec)
    (error (compatfmt "~@<Can't reliably convert wild pathname ~3i~_~S~@:>") pathspec))
   ((directory-pathname-p pathspec)
    pathspec)
   (t
    (make-pathname* :directory (append (or (normalize-pathname-directory-component
                                            (pathname-directory pathspec))
                                           (list :relative))
                                       (list (file-namestring pathspec)))
                    :name nil :type nil :version nil :defaults pathspec))))


;;; Wildcard pathnames
(defparameter *wild* (or #+cormanlisp "*" :wild))
(defparameter *wild-file*
  (make-pathname :directory nil :name *wild* :type *wild*
                  :version (or #-(or abcl xcl) *wild*)))
(defparameter *wild-directory*
  (make-pathname* :directory `(:relative ,(or #+gcl<2.7 "*" *wild*))
                  :name nil :type nil :version nil))
(defparameter *wild-inferiors*
  (make-pathname* :directory `(:relative ,(or #+gcl<2.7 "**" :wild-inferiors))
                  :name nil :type nil :version nil))
(defparameter *wild-path*
  (merge-pathnames *wild-file* *wild-inferiors*))

(defun* wilden (path)
  (merge-pathnames* *wild-path* path))


;;; Probing the filesystem
(defun* nil-pathname (&optional (defaults *default-pathname-defaults*))
  (make-pathname :directory nil :name nil :type nil :version nil :device nil :host nil
                 :defaults defaults)) ;; shouldn't matter

(defmacro with-pathname-defaults ((&optional defaults) &body body)
  `(let ((*default-pathname-defaults* ,(or defaults (nil-pathname)))) ,@body))

(defun truename* (p)
  ;; avoids both logical-pathname merging and physical resolution issues
  (ignore-errors (with-pathname-defaults () (truename p))))

(defun* probe-file* (p)
  "when given a pathname P, probes the filesystem for a file or directory
with given pathname and if it exists return its truename."
  (with-pathname-defaults () ;; avoids logical-pathname issues on some implementations
    (etypecase p
      (null nil)
      (string (probe-file* (parse-namestring p)))
      (pathname (unless (wild-pathname-p p)
                  #.(or #+(or allegro clozure cmu cormanlisp ecl lispworks mkcl sbcl scl)
                        '(probe-file p)
                        #+clisp (aif (find-symbol* '#:probe-pathname :ext nil)
                                     `(ignore-errors (,it p)))
                        #+gcl<2.7
                        '(or (probe-file p)
                          (and (directory-pathname-p p)
                           (ignore-errors
                            (ensure-directory-pathname
                             (truename* (subpathname (ensure-directory-pathname p) "."))))))
                        '(truename* p)))))))

(defun* safe-file-write-date (pathname)
  ;; If FILE-WRITE-DATE returns NIL, it's possible that
  ;; the user or some other agent has deleted an input file.
  ;; Also, generated files will not exist at the time planning is done
  ;; and calls compute-action-stamp which calls safe-file-write-date.
  ;; So it is very possible that we can't get a valid file-write-date,
  ;; and we can survive and we will continue the planning
  ;; as if the file were very old.
  ;; (or should we treat the case in a different, special way?)
  (and pathname (probe-file* pathname) (ignore-errors (file-write-date pathname))))

(defun* directory* (pathname-spec &rest keys &key &allow-other-keys)
  (apply 'directory pathname-spec
         (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                             #+clozure '(:follow-links nil)
                             #+clisp '(:circle t :if-does-not-exist :ignore)
                             #+(or cmu scl) '(:follow-links nil :truenamep nil)
                             #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl nil)
                                      '(:resolve-symlinks nil))))))

(defun* filter-logical-directory-results (directory entries merger)
  (if (typep directory 'logical-pathname)
      ;; Try hard to not resolve logical-pathname into physical pathnames;
      ;; otherwise logical-pathname users/lovers will be disappointed.
      ;; If directory* could use some implementation-dependent magic,
      ;; we will have logical pathnames already; otherwise,
      ;; we only keep pathnames for which specifying the name and
      ;; translating the LPN commute.
      (loop :for f :in entries
        :for p = (or (and (typep f 'logical-pathname) f)
                     (let* ((u (ignore-errors (funcall merger f))))
                       ;; The first u avoids a cumbersome (truename u) error.
                       ;; At this point f should already be a truename,
                       ;; but isn't quite in CLISP, for it doesn't have :version :newest
                       (and u (equal (truename* u) (truename* f)) u)))
        :when p :collect p)
      entries))

(defun* directory-files (directory &optional (pattern *wild-file*))
  (let ((dir (pathname directory)))
    (when (typep dir 'logical-pathname)
      ;; Because of the filtering we do below,
      ;; logical pathnames have restrictions on wild patterns.
      ;; Not that the results are very portable when you use these patterns on physical pathnames.
      (when (wild-pathname-p dir)
        (error "Invalid wild pattern in logical directory ~S" directory))
      (unless (member (pathname-directory pattern) '(() (:relative)) :test 'equal)
        (error "Invalid file pattern ~S for logical directory ~S" pattern directory))
      (setf pattern (make-pathname-logical pattern (pathname-host dir))))
    (let ((entries (ignore-errors (directory* (merge-pathnames* pattern dir)))))
      (filter-logical-directory-results
       directory entries
       #'(lambda (f)
           (make-pathname :defaults dir
                          :name (make-pathname-component-logical (pathname-name f))
                          :type (make-pathname-component-logical (pathname-type f))
                          :version (make-pathname-component-logical (pathname-version f))))))))

(defun* subdirectories (directory)
  (let* ((directory (ensure-directory-pathname directory))
         #-(or abcl cormanlisp genera xcl)
         (wild (merge-pathnames*
                #-(or abcl allegro cmu lispworks sbcl scl xcl)
                *wild-directory*
                #+(or abcl allegro cmu lispworks sbcl scl xcl) "*.*"
                directory))
         (dirs
          #-(or abcl cormanlisp genera xcl)
          (ignore-errors
            (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                     #+mcl '(:directories t))))
          #+(or abcl xcl) (system:list-directory directory)
          #+cormanlisp (cl::directory-subdirs directory)
          #+genera (fs:directory-list directory))
         #+(or abcl allegro cmu genera lispworks sbcl scl xcl)
         (dirs (loop :for x :in dirs
                 :for d = #+(or abcl xcl) (extensions:probe-directory x)
                          #+allegro (excl:probe-directory x)
                          #+(or cmu sbcl scl) (directory-pathname-p x)
                          #+genera (getf (cdr x) :directory)
                          #+lispworks (lw:file-directory-p x)
                 :when d :collect #+(or abcl allegro xcl) d
                                  #+genera (ensure-directory-pathname (first x))
                                  #+(or cmu lispworks sbcl scl) x)))
    (filter-logical-directory-results
     directory dirs
     (let ((prefix (or (normalize-pathname-directory-component (pathname-directory directory))
                       '(:absolute)))) ; because allegro returns NIL for #p"FOO:"
       #'(lambda (d)
           (let ((dir (normalize-pathname-directory-component (pathname-directory d))))
             (and (consp dir) (consp (cdr dir))
                  (make-pathname
                   :defaults directory :name nil :type nil :version nil
                   :directory (append prefix (make-pathname-component-logical (last dir)))))))))))

(defun* collect-sub*directories (directory collectp recursep collector)
  (when (funcall collectp directory)
    (funcall collector directory))
  (dolist (subdir (subdirectories directory))
    (when (funcall recursep subdir)
      (collect-sub*directories subdir collectp recursep collector))))



;;; Parsing filenames and lists thereof
(defun* component-name-to-pathname-components (s &key force-directory force-relative)
  "Splits the path string S, returning three values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings, suitable for
   use with MAKE-PATHNAME when prepended with the flag
   value.
A filename with type extension, possibly NIL in the
   case of a directory pathname.
FORCE-DIRECTORY forces S to be interpreted as a directory
pathname \(third return value will be NIL, final component
of S will be treated as part of the directory path.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative
pathnames."
  (check-type s string)
  (when (find #\: s)
    (error (compatfmt "~@<A portable ASDF pathname designator cannot include a #\: character: ~3i~_~S~@:>") s))
  (let* ((components (split-string s :separator "/"))
         (last-comp (car (last components))))
    (multiple-value-bind (relative components)
        (if (equal (first components) "")
            (if (equal (first-char s) #\/)
                (progn
                  (when force-relative
                    (error (compatfmt "~@<Absolute pathname designator not allowed: ~3i~_~S~@:>") s))
                  (values :absolute (cdr components)))
                (values :relative nil))
          (values :relative components))
      (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal)) components))
      (setf components (substitute :back ".." components :test #'equal))
      (cond
        ((equal last-comp "")
         (values relative components nil)) ; "" already removed
        (force-directory
         (values relative components nil))
        (t
         (values relative (butlast components) last-comp))))))

(defun* split-name-type (filename)
  (let ((unspecific
         ;; Giving :unspecific as argument to make-pathname is not portable.
         ;; See CLHS make-pathname and 19.2.2.2.3.
         ;; We only use it on implementations that support it,
         #+(or abcl allegro clozure cmu gcl genera lispworks mkcl sbcl scl xcl) :unspecific
         #+(or clisp ecl #|These haven't been tested:|# cormanlisp mcl) nil))
    (destructuring-bind (name &optional (type unspecific))
        (split-string filename :max 2 :separator ".")
      (if (equal name "")
          (values filename unspecific)
          (values name type)))))

(defun* coerce-pathname (name &key type defaults)
  "coerce NAME into a PATHNAME.
When given a string, portably decompose it into a relative pathname:
#\\/ separates subdirectories. The last #\\/-separated string is as follows:
if TYPE is NIL, its last #\\. if any separates name and type from from type;
if TYPE is a string, it is the type, and the whole string is the name;
if TYPE is :DIRECTORY, the string is a directory component;
if the string is empty, it's a directory.
Any directory named .. is read as :BACK.
Host, device and version components are taken from DEFAULTS."
  ;; The defaults are required notably because they provide the default host
  ;; to the below make-pathname, which may crucially matter to people using
  ;; merge-pathnames with non-default hosts,  e.g. for logical-pathnames.
  ;; NOTE that the host and device slots will be taken from the defaults,
  ;; but that should only matter if you later merge relative pathnames with
  ;; CL:MERGE-PATHNAMES instead of ASDF:MERGE-PATHNAMES*
  (etypecase name
    ((or null pathname)
     name)
    (symbol
     (coerce-pathname (string-downcase name) :type type :defaults defaults))
    (string
     (multiple-value-bind (relative path filename)
         (component-name-to-pathname-components name :force-directory (eq type :directory)
                                                :force-relative t)
       (multiple-value-bind (name type)
           (cond
             ((or (eq type :directory) (null filename))
              (values nil nil))
             (type
              (values filename type))
             (t
              (split-name-type filename)))
         (apply 'make-pathname* :directory (cons relative path) :name name :type type
                (when defaults `(:defaults ,defaults))))))))

(defun* subpathname (pathname subpath &key type)
  (and pathname (merge-pathnames* (coerce-pathname subpath :type type)
                                  (pathname-directory-pathname pathname))))

(defun* subpathname* (pathname subpath &key type)
  (and pathname
       (subpathname (ensure-directory-pathname pathname) subpath :type type)))

(defun* absolute-pathname-p (pathspec)
  (and (typep pathspec '(or pathname string))
       (eq :absolute (car (normalize-pathname-directory-component
                           (pathname-directory (pathname pathspec)))))))

(defun* ensure-pathname* (x want-absolute want-directory fmt &rest args)
  (when (plusp (length x))
    (let ((p (if want-directory (ensure-directory-pathname x) (pathname x))))
      (when want-absolute
        (unless (absolute-pathname-p p)
          (cerror "ignore relative pathname"
                  "Invalid relative pathname ~A~@[ ~?~]" x fmt args)
          (return-from ensure-pathname* nil)))
      p)))

(defun* hidden-file-p (pathname)
  (equal (first-char (pathname-name pathname)) #\.))


;;; Pathname host and its root
(defun* pathname-root (pathname)
  (make-pathname* :directory '(:absolute)
                  :name nil :type nil :version nil
                  :defaults pathname ;; host device, and on scl, *some*
                  ;; scheme-specific parts: port username password, not others:
                  . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

(defun* pathname-host-pathname (pathname)
  (make-pathname :directory nil
                 :name nil :type nil :version nil :device nil
                 :defaults pathname ;; host device, and on scl, *some*
                  ;; scheme-specific parts: port username password, not others:
                  . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

#-scl
(defun* directory-separator-for-host (&optional (pathname *default-pathname-defaults*))
  (let ((foo (make-pathname* :directory '(:absolute "FOO") :defaults pathname)))
    (last-char (namestring foo))))

#-scl
(defun* directorize-pathname-host-device (pathname)
  (let* ((root (pathname-root pathname))
         (wild-root (wilden root))
         (absolute-pathname (merge-pathnames* pathname root))
         (separator (directory-separator-for-host root))
         (root-namestring (namestring root))
         (root-string
          (substitute-if #\/
                         #'(lambda (x) (or (eql x #\:)
                                           (eql x separator)))
                         root-namestring)))
    (multiple-value-bind (relative path filename)
        (component-name-to-pathname-components root-string :force-directory t)
      (declare (ignore relative filename))
      (let ((new-base
             (make-pathname* :defaults root :directory `(:absolute ,@path))))
        (translate-pathname absolute-pathname wild-root (wilden new-base))))))

#+scl
(defun* directorize-pathname-host-device (pathname)
  (let ((scheme (ext:pathname-scheme pathname))
        (host (pathname-host pathname))
        (port (ext:pathname-port pathname))
        (directory (pathname-directory pathname)))
    (flet ((specificp (x) (and x (not (eq x :unspecific)))))
      (if (or (specificp port)
              (and (specificp host) (plusp (length host)))
              (specificp scheme))
        (let ((prefix ""))
          (when (specificp port)
            (setf prefix (format nil ":~D" port)))
          (when (and (specificp host) (plusp (length host)))
            (setf prefix (strcat host prefix)))
          (setf prefix (strcat ":" prefix))
          (when (specificp scheme)
            (setf prefix (strcat scheme prefix)))
          (assert (and directory (eq (first directory) :absolute)))
          (make-pathname* :directory `(:absolute ,prefix ,@(rest directory))
                          :defaults pathname)))
    pathname)))


;;; Resolving symlinks somewhat
(defun* truenamize (pathname &optional (defaults *default-pathname-defaults*))
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep pathname '(or null logical-pathname)) (return pathname))
    (let ((p (merge-pathnames* pathname defaults)))
      (when (typep p 'logical-pathname) (return p))
      (let ((found (probe-file* p)))
        (when found (return found)))
      (unless (absolute-pathname-p p)
        (let ((true-defaults (truename* defaults)))
          (when true-defaults
            (setf p (merge-pathnames pathname true-defaults)))))
      (unless (absolute-pathname-p p) (return p))
      (let ((sofar (probe-file* (pathname-root p))))
        (unless sofar (return p))
        (flet ((solution (directories)
                 (merge-pathnames*
                  (make-pathname* :host nil :device nil
                                  :directory `(:relative ,@directories)
                                  :name (pathname-name p)
                                  :type (pathname-type p)
                                  :version (pathname-version p))
                  sofar)))
          (loop :with directory = (normalize-pathname-directory-component
                                   (pathname-directory p))
            :for component :in (cdr directory)
            :for rest :on (cdr directory)
            :for more = (probe-file*
                         (merge-pathnames*
                          (make-pathname* :directory `(:relative ,component))
                          sofar)) :do
            (if more
                (setf sofar more)
                (return (solution rest)))
            :finally
            (return (solution nil))))))))

(defun* resolve-symlinks (path)
  #-allegro (truenamize path)
  #+allegro (if (typep path 'logical-pathname)
                path
                (excl:pathname-resolve-symbolic-links path)))

(defun* resolve-symlinks* (path)
  (if *resolve-symlinks*
      (and path (resolve-symlinks path))
      path))


;;; absolute vs relative
(defun* ensure-pathname-absolute (path)
  (cond
    ((absolute-pathname-p path) path)
    ((stringp path) (ensure-pathname-absolute (pathname path)))
    ((not (pathnamep path)) (error "not a valid pathname designator ~S" path))
    (t (let ((resolved (resolve-symlinks path)))
         (assert (absolute-pathname-p resolved))
         resolved))))

(defun* relativize-directory-component (directory-component)
  (let ((directory (normalize-pathname-directory-component directory-component)))
    (cond
      ((stringp directory)
       (list :relative directory))
      ((eq (car directory) :absolute)
       (cons :relative (cdr directory)))
      (t
       directory))))

(defun* relativize-pathname-directory (pathspec)
  (let ((p (pathname pathspec)))
    (make-pathname*
     :directory (relativize-directory-component (pathname-directory p))
     :defaults p)))


;;; Simple filesystem operations
(defun* ensure-all-directories-exist (pathnames)
   (dolist (pathname pathnames)
     (ensure-directories-exist (translate-logical-pathname pathname))))

(defun* rename-file-overwriting-target (source target)
  #+clisp ;; But for a bug in CLISP 2.48, we should use :if-exists :overwrite and be atomic
  (posix:copy-file source target :method :rename)
  #-clisp
  (rename-file source target
               #+clozure :if-exists #+clozure :rename-and-delete))

(defun* delete-file-if-exists (x)
  (when (and x (probe-file* x))
    (delete-file x)))

;;; Translate a pathname
(defun* translate-pathname* (path absolute-source destination &optional root source)
  (declare (ignore source))
  (cond
    ((functionp destination)
     (funcall destination path absolute-source))
    ((eq destination t)
     path)
    ((not (pathnamep destination))
     (error "Invalid destination"))
    ((not (absolute-pathname-p destination))
     (translate-pathname path absolute-source (merge-pathnames* destination root)))
    (root
     (translate-pathname (directorize-pathname-host-device path) absolute-source destination))
    (t
     (translate-pathname path absolute-source destination))))


;;; Native vs Lisp syntax

(defun native-namestring (x)
  "From a CL pathname, a namestring suitable for use by the OS shell"
  (let ((p (pathname x)))
    #+clozure (let ((*default-pathname-defaults* #p"")) (ccl:native-translated-namestring p)) ; see ccl bug 978
    #+(or cmu scl) (ext:unix-namestring p nil)
    #+sbcl (sb-ext:native-namestring p)
    #-(or clozure cmu sbcl scl) (namestring p)))

(defun parse-native-namestring (x)
  "From a native namestring suitable for use by the OS shell, a CL pathname"
  (check-type x string)
  #+clozure (ccl:native-to-pathname x)
  #+sbcl (sb-ext:parse-native-namestring x)
  #-(or clozure sbcl) (parse-namestring x))


;;; Temporary pathnames
(defun* add-pathname-suffix (pathname suffix)
  (make-pathname :name (strcat (pathname-name pathname) suffix)
                 :defaults pathname))

(defun* tmpize-pathname (x)
  (add-pathname-suffix x "-ASDF-TMP"))

(defun* call-with-staging-pathname (pathname fun)
  "Calls fun with a staging pathname, and atomically
renames the staging pathname to the pathname in the end.
Note: this protects only against failure of the program,
not against concurrent attempts.
For the latter case, we ought pick random suffix and atomically open it."
  (let* ((pathname (pathname pathname))
         (staging (tmpize-pathname pathname)))
    (unwind-protect
         (multiple-value-prog1
             (funcall fun staging)
           (rename-file-overwriting-target staging pathname))
      (when (probe-file* staging)
        (delete-file staging)))))

(defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
  `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body)))

;;; Basic pathnames
(defun* load-pathname ()
  (resolve-symlinks* (or *load-pathname* *compile-file-pathname*)))

(defun* default-directory () ;; A default absolute directory when all else fails.
  (pathname-root (nil-pathname)))

(defun* physical-pathname-p (x)
  (and (pathnamep x) (not (typep x 'logical-pathname))))

(defun* sane-physical-pathname (defaults &key (keep t) fallback must-exist)
  (flet ((sanitize (x)
           (setf x (and x (ignore-errors (translate-logical-pathname x))))
           (when (pathnamep x)
             (setf x
                   (ecase keep
                     ((t) x)
                     ((:directory) (pathname-directory-pathname x))
                     ((:root) (pathname-root x))
                     ((:host) (pathname-host-pathname x))
                     ((nil) (nil-pathname x))))
             (when must-exist ;; CCL's probe-file will choke if d-p-d is logical
               (setf x (and (probe-file* x) x)))
             (and (physical-pathname-p x) x))))
    (or (sanitize defaults)
        (when fallback
          (or (sanitize (ignore-errors (user-homedir-pathname)))
              (sanitize (nil-pathname))))
        (error "Could not find a sanitize ~S ~:[~;or a fallback ~] into a physical pathname"
               defaults fallback))))


;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13

#-(or clisp genera) ; CLISP doesn't need it, and READ-SEQUENCE annoys old Genera.
(progn
(defparameter *link-initial-dword* 76)
(defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

(defun* read-null-terminated-string (s)
  (with-output-to-string (out)
    (loop :for code = (read-byte s)
      :until (zerop code)
      :do (write-char (code-char code) out))))

(defun* read-little-endian (s &optional (bytes 4))
  (loop :for i :from 0 :below bytes
    :sum (ash (read-byte s) (* 8 i))))

(defun* parse-file-location-info (s)
  (let ((start (file-position s))
        (total-length (read-little-endian s))
        (end-of-header (read-little-endian s))
        (fli-flags (read-little-endian s))
        (local-volume-offset (read-little-endian s))
        (local-offset (read-little-endian s))
        (network-volume-offset (read-little-endian s))
        (remaining-offset (read-little-endian s)))
    (declare (ignore total-length end-of-header local-volume-offset))
    (unless (zerop fli-flags)
      (cond
        ((logbitp 0 fli-flags)
          (file-position s (+ start local-offset)))
        ((logbitp 1 fli-flags)
          (file-position s (+ start
                              network-volume-offset
                              #x14))))
      (strcat (read-null-terminated-string s)
              (progn
                (file-position s (+ start remaining-offset))
                (read-null-terminated-string s))))))

(defun* parse-windows-shortcut (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (handler-case
        (when (and (= (read-little-endian s) *link-initial-dword*)
                   (let ((header (make-array (length *link-guid*))))
                     (read-sequence header s)
                     (equalp header *link-guid*)))
          (let ((flags (read-little-endian s)))
            (file-position s 76)        ;skip rest of header
            (when (logbitp 0 flags)
              ;; skip shell item id list
              (let ((length (read-little-endian s 2)))
                (file-position s (+ length (file-position s)))))
            (cond
              ((logbitp 1 flags)
                (parse-file-location-info s))
              (t
                (when (logbitp 2 flags)
                  ;; skip description string
                  (let ((length (read-little-endian s 2)))
                    (file-position s (+ length (file-position s)))))
                (when (logbitp 3 flags)
                  ;; finally, our pathname
                  (let* ((length (read-little-endian s 2))
                         (buffer (make-array length)))
                    (read-sequence buffer s)
                    (map 'string #'code-char buffer)))))))
      (end-of-file (c)
        (declare (ignore c))
        nil)))))
