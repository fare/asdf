;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp pathnames

(asdf/package:define-package :asdf/pathname
  (:recycle :asdf/pathname :asdf)
  #+gcl<2.7 (:shadowing-import-from :system :*load-pathname*) ;; GCL 2.6 sucks
  (:use :common-lisp :asdf/package :asdf/compatibility :asdf/utility)
  (:export
   #:*resolve-symlinks*
   ;; Making and merging pathnames, portably
   #:normalize-pathname-directory-component #:denormalize-pathname-directory-component
   #:pathname-equal
   #:merge-pathname-directory-components #:make-pathname* #:*unspecific-pathname-type*
   #:make-pathname-component-logical #:make-pathname-logical
   #:merge-pathnames*
   ;; Directories
   #:pathname-directory-pathname #:pathname-parent-directory-pathname
   #:directory-pathname-p #:ensure-directory-pathname #:file-pathname-p
   ;; Absolute vs relative pathnames
   #:ensure-pathname-absolute
   #:relativize-directory-component #:relativize-pathname-directory
   ;; Parsing filenames and lists thereof
   #:component-name-to-pathname-components
   #:split-name-type #:parse-unix-namestring #:unix-namestring
   #:split-unix-namestring-directory-components
   #:subpathname #:subpathname* #:subpathp
   ;; Resolving symlinks somewhat
   #:truenamize #:resolve-symlinks #:resolve-symlinks*
   ;; Wildcard pathnames
   #:*wild* #:*wild-file* #:*wild-directory* #:*wild-inferiors* #:*wild-path* #:wilden
   ;; Pathname host and its root
   #:absolute-pathname-p #:relative-pathname-p #:hidden-pathname-p
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
   ;; temporary
   #:add-pathname-suffix #:tmpize-pathname
   #:call-with-staging-pathname #:with-staging-pathname
   ;; physical pathnames
   #:logical-pathname-p #:physical-pathname-p #:sane-physical-pathname #:root-pathname
   ;; Windows shortcut support
   #:read-null-terminated-string #:read-little-endian
   #:parse-file-location-info #:parse-windows-shortcut
   ;; Checking constraints
   #:ensure-pathname
   #:absolutize-pathnames
   ;; Output translations
   #:*output-translation-function*))

(in-package :asdf/pathname)

;;; User-visible parameters
(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.

Defaults to T.")


;;; Normalizing pathnames across implementations

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

;; Giving :unspecific as :type argument to make-pathname is not portable.
;; See CLHS make-pathname and 19.2.2.2.3.
;; This will be :unspecific if supported, or NIL if not.
(defparameter *unspecific-pathname-type*
  #+(or abcl allegro clozure cmu gcl genera lispworks mkcl sbcl scl xcl) :unspecific
  #+(or clisp ecl #|These haven't been tested:|# cormanlisp mcl) nil)

(defun* make-pathname* (&rest keys &key (directory nil directoryp)
                              host (device () devicep) name type version defaults
                              #+scl &allow-other-keys)
  "Takes arguments like CL:MAKE-PATHNAME in the CLHS, and
   tries hard to make a pathname that will actually behave as documented,
   despite the peculiarities of each implementation"
  (declare (ignorable host device devicep name type version defaults))
  (apply 'make-pathname
         (append
          #+allegro (when (and devicep (null device)) `(:device :unspecific))
          (when directoryp
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


;;; Some pathname predicates

(defun* pathname-equal (p1 p2)
  (when (stringp p1) (setf p1 (pathname p1)))
  (when (stringp p2) (setf p2 (pathname p2)))
  (flet ((normalize-component (x)
           (unless (member x '(nil :unspecific :newest (:relative)) :test 'equal)
             x)))
    (macrolet ((=? (&rest accessors)
                 (flet ((frob (x)
                          (reduce 'list (cons 'normalize-component accessors)
                                  :initial-value x :from-end t)))
                   `(equal ,(frob 'p1) ,(frob 'p2)))))
      (or (and (null p1) (null p2))
          (and (pathnamep p1) (pathnamep p2)
               (and (=? pathname-host)
                    (=? pathname-device)
                    (=? normalize-pathname-directory-component pathname-directory)
                    (=? pathname-name)
                    (=? pathname-type)
                    (=? pathname-version)))))))

(defun* logical-pathname-p (x)
  (typep x 'logical-pathname))

(defun* physical-pathname-p (x)
  (and (pathnamep x) (not (logical-pathname-p x))))

(defun* absolute-pathname-p (pathspec)
  "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing an :ABSOLUTE directory component, return the (parsed) pathname.
Otherwise return NIL"
  (and pathspec
       (typep pathspec '(or null pathname string))
       (let ((pathname (pathname pathspec)))
         (and (eq :absolute (car (normalize-pathname-directory-component
                                  (pathname-directory pathname))))
              pathname))))

(defun* relative-pathname-p (pathspec)
  "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing a :RELATIVE or NIL directory component, return the (parsed) pathname.
Otherwise return NIL"
  (and pathspec
       (typep pathspec '(or null pathname string))
       (let* ((pathname (pathname pathspec))
              (directory (normalize-pathname-directory-component
                          (pathname-directory pathname))))
         (when (or (null directory) (eq :relative (car directory)))
           pathname))))

(defun* hidden-pathname-p (pathname)
  "Return a boolean that is true if the pathname is hidden as per Unix style,
i.e. its name starts with a dot."
  (and pathname (equal (first-char (pathname-name pathname)) #\.)))


;;;; merging pathnames
(defun* merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
  "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that
if the SPECIFIED pathname does not have an absolute directory,
then the HOST and DEVICE both come from the DEFAULTS, whereas
if the SPECIFIED pathname does have an absolute directory,
then the HOST and DEVICE both come from the SPECIFIED.
This is what users want on a modern Unix or Windows operating system,
unlike the MERGE-PATHNAME behavior.
Also, if either argument is NIL, then the other argument is returned unmodified;
this is unlike MERGE-PATHNAME which always merges with a pathname,
by default *DEFAULT-PATHNAME-DEFAULTS*, which cannot be NIL."
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
               (if (logical-pathname-p p) #'make-pathname-component-logical #'identity)))
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

(defun* file-pathname-p (pathname)
  "Does PATHNAME represent a file, i.e. has a non-null NAME component?

Accepts NIL, a string (converted through PARSE-NAMESTRING) or a PATHNAME.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing file.

Returns the (parsed) PATHNAME when true"
  (when pathname
    (let* ((pathname (pathname pathname))
           (name (pathname-name pathname)))
      (when (not (member name '(nil :unspecific "") :test 'equal))
        pathname))))

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
(defparameter *wild-directory-component* (or #+gcl<2.7 "*" :wild))
(defparameter *wild-inferiors-component* (or #+gcl<2.7 "**" :wild-inferiors))
(defparameter *wild-file*
  (make-pathname :directory nil :name *wild* :type *wild*
                 :version (or #-(or allegro abcl xcl) *wild*)))
(defparameter *wild-directory*
  (make-pathname* :directory `(:relative ,*wild-directory-component*)
                  :name nil :type nil :version nil))
(defparameter *wild-inferiors*
  (make-pathname* :directory `(:relative ,*wild-inferiors-component*)
                  :name nil :type nil :version nil))
(defparameter *wild-path*
  (merge-pathnames* *wild-file* *wild-inferiors*))

(defun* wilden (path)
  (merge-pathnames* *wild-path* path))


;;; Probing the filesystem
(defun* nil-pathname (&optional (defaults *default-pathname-defaults*))
  ;; 19.2.2.2.1 says a NIL host can mean a default host;
  ;; see also "valid physical pathname host" in the CLHS glossary, that suggests
  ;; strings and lists of strings or :unspecific
  ;; But CMUCL decides to die on NIL.
  (make-pathname* :directory nil :name nil :type nil :version nil :device nil
                  :host (or #+cmu lisp::*unix-host*)
                  ;; the default shouldn't matter, but we really want something physical
                  :defaults defaults))

(defmacro with-pathname-defaults ((&optional defaults) &body body)
  `(let ((*default-pathname-defaults* ,(or defaults '(nil-pathname)))) ,@body))

(defun* truename* (p)
  ;; avoids both logical-pathname merging and physical resolution issues
  (and p (ignore-errors (with-pathname-defaults () (truename p)))))

(defun* probe-file* (p &key truename)
  "when given a pathname P (designated by a string as per PARSE-NAMESTRING),
probes the filesystem for a file or directory with given pathname.
If it exists, return its truename is ENSURE-PATHNAME is true,
or the original (parsed) pathname if it is false (the default)."
  (with-pathname-defaults () ;; avoids logical-pathname issues on some implementations
    (etypecase p
      (null nil)
      (string (probe-file* (parse-namestring p) :truename truename))
      (pathname (unless (wild-pathname-p p)
                  (let ((foundtrue
                          #.(or #+(or allegro clozure cmu cormanlisp ecl lispworks mkcl sbcl scl)
                                '(probe-file p)
                                #+clisp (if-let (it (find-symbol* '#:probe-pathname :ext nil))
                                          `(ignore-errors (,it p)))
                                #+gcl<2.7
                                '(or (probe-file p)
                                  (and (directory-pathname-p p)
                                   (ignore-errors
                                    (ensure-directory-pathname
                                     (truename* (subpathname
                                                 (ensure-directory-pathname p) "."))))))
                                '(truename* p))))
                    (cond
                      (truename foundtrue)
                      (foundtrue p)
                      (t nil))))))))

(defun* safe-file-write-date (pathname)
  ;; If FILE-WRITE-DATE returns NIL, it's possible that
  ;; the user or some other agent has deleted an input file.
  ;; Also, generated files will not exist at the time planning is done
  ;; and calls compute-action-stamp which calls safe-file-write-date.
  ;; So it is very possible that we can't get a valid file-write-date,
  ;; and we can survive and we will continue the planning
  ;; as if the file were very old.
  ;; (or should we treat the case in a different, special way?)
  (and (probe-file* pathname) (ignore-errors (file-write-date pathname))))

(defun* directory* (pathname-spec &rest keys &key &allow-other-keys)
  (apply 'directory pathname-spec
         (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                             #+clozure '(:follow-links nil)
                             #+clisp '(:circle t :if-does-not-exist :ignore)
                             #+(or cmu scl) '(:follow-links nil :truenamep nil)
                             #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl nil)
                                      '(:resolve-symlinks nil))))))

(defun* filter-logical-directory-results (directory entries merger)
  (if (logical-pathname-p directory)
      ;; Try hard to not resolve logical-pathname into physical pathnames;
      ;; otherwise logical-pathname users/lovers will be disappointed.
      ;; If directory* could use some implementation-dependent magic,
      ;; we will have logical pathnames already; otherwise,
      ;; we only keep pathnames for which specifying the name and
      ;; translating the LPN commute.
      (loop :for f :in entries
        :for p = (or (and (logical-pathname-p f) f)
                     (let* ((u (ignore-errors (funcall merger f))))
                       ;; The first u avoids a cumbersome (truename u) error.
                       ;; At this point f should already be a truename,
                       ;; but isn't quite in CLISP, for it doesn't have :version :newest
                       (and u (equal (truename* u) (truename* f)) u)))
        :when p :collect p)
      entries))

(defun* directory-files (directory &optional (pattern *wild-file*))
  (let ((dir (pathname directory)))
    (when (logical-pathname-p dir)
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
(defun* split-unix-namestring-directory-components
    (unix-namestring &key ensure-directory dot-dot)
  "Splits the path string UNIX-NAMESTRING, returning four values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings and keywords, suitable for
   use with MAKE-PATHNAME when prepended with the flag value.
   Directory components with an empty name or the name . are removed.
   Any directory named .. is read as DOT-DOT, or :BACK if it's NIL (not :UP).
A last-component, either a file-namestring including type extension,
   or NIL in the case of a directory pathname.
A flag that is true iff the unix-style-pathname was just
   a file-namestring without / path specification.
ENSURE-DIRECTORY forces the namestring to be interpreted as a directory pathname:
the third return value will be NIL, and final component of the namestring
will be treated as part of the directory path.

An empty string is thus read as meaning a pathname object with all fields nil.

Note that : characters will NOT be interpreted as host specification.
Absolute pathnames are only appropriate on Unix-style systems.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative pathnames."
  (check-type unix-namestring string)
  (check-type dot-dot (member nil :back :up))
  (if (and (not (find #\/ unix-namestring)) (not ensure-directory)
           (plusp (length unix-namestring)))
      (values :relative () unix-namestring t)
      (let* ((components (split-string unix-namestring :separator "/"))
             (last-comp (car (last components))))
        (multiple-value-bind (relative components)
            (if (equal (first components) "")
                (if (equal (first-char unix-namestring) #\/)
                    (values :absolute (cdr components))
                    (values :relative nil))
                (values :relative components))
          (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal))
                                      components))
          (setf components (substitute (or dot-dot :back) ".." components :test #'equal))
          (cond
            ((equal last-comp "")
             (values relative components nil nil)) ; "" already removed from components
            (ensure-directory
             (values relative components nil nil))
            (t
             (values relative (butlast components) last-comp nil)))))))

(defun* split-name-type (filename)
  "Split a filename into two values NAME and TYPE that are returned.
We assume filename has no directory component.
The last . if any separates name and type from from type,
except that if there is only one . and it is in first position,
the whole filename is the NAME with an empty type.
NAME is always a string.
For an empty type, *UNSPECIFIC-PATHNAME-TYPE* is returned."
  (check-type filename string)
  (assert (plusp (length filename)))
  (destructuring-bind (name &optional (type *unspecific-pathname-type*))
      (split-string filename :max 2 :separator ".")
    (if (equal name "")
        (values filename *unspecific-pathname-type*)
        (values name type))))

(defun* parse-unix-namestring (name &rest keys &key type defaults dot-dot ensure-directory
                                    &allow-other-keys)
  "Coerce NAME into a PATHNAME using standard Unix syntax.

Unix syntax is used whether or not the underlying system is Unix;
on such non-Unix systems it is only usable but for relative pathnames;
but especially to manipulate relative pathnames portably, it is of crucial
to possess a portable pathname syntax independent of the underlying OS.
This is what PARSE-UNIX-NAMESTRING provides, and why we use it in ASDF.

When given a PATHNAME object, just return it untouched.
When given NIL, just return NIL.
When given a non-null SYMBOL, first downcase its name and treat it as a string.
When given a STRING, portably decompose it into a pathname as below.

#\\/ separates directory components.

The last #\\/-separated substring is interpreted as follows:
1- If TYPE is :DIRECTORY or ENSURE-DIRECTORY is true,
 the string is made the last directory component, and NAME and TYPE are NIL.
 if the string is empty, it's the empty pathname with all slots NIL.
2- If TYPE is NIL, the substring is file-namestring, and its NAME and TYPE
 are separated by SPLIT-NAME-TYPE.
3- If TYPE is a string, it is the given TYPE, and the whole string is the NAME.

Directory components with an empty name the name . are removed.
Any directory named .. is read as DOT-DOT,
which must be one of :BACK or :UP and defaults to :BACK.

HOST, DEVICE and VERSION components are taken from DEFAULTS,
which itself defaults to (ROOT-PATHNAME), also used if DEFAULTS in NIL.
No host or device can be specified in the string itself,
which makes it unsuitable for absolute pathnames outside Unix.

For relative pathnames, these components (and hence the defaults) won't matter
if you use MERGE-PATHNAMES* but will matter if you use MERGE-PATHNAMES,
which is an important reason to always use MERGE-PATHNAMES*.

Arbitrary keys are accepted, and the parse result is passed to ENSURE-PATHNAME
with those keys, removing TYPE DEFAULTS and DOT-DOT.
When you're manipulating pathnames that are supposed to make sense portably
even though the OS may not be Unixish, we recommend you use :WANT-RELATIVE T
to throw an error if the pathname is absolute"
  (block nil
    (check-type type (or null string (eql :directory)))
    (when ensure-directory
      (setf type :directory))
    (etypecase name
      ((or null pathname) (return name))
      (symbol
       (setf name (string-downcase name)))
      (string))
    (multiple-value-bind (relative path filename file-only)
        (split-unix-namestring-directory-components
         name :dot-dot dot-dot :ensure-directory (eq type :directory))
      (multiple-value-bind (name type)
          (cond
            ((or (eq type :directory) (null filename))
             (values nil nil))
            (type
             (values filename type))
            (t
             (split-name-type filename)))
        (apply 'ensure-pathname
               (make-pathname*
                :directory (unless file-only (cons relative path))
                :name name :type type
                :defaults (or defaults (nil-pathname)))
               (remove-plist-keys '(:type :dot-dot :defaults) keys))))))

(defun* unix-namestring (pathname)
  "Given a non-wild PATHNAME, return a Unix-style namestring for it.
If the PATHNAME is NIL or a STRING, return it unchanged.

This only considers the DIRECTORY, NAME and TYPE components of the pathname.
This is a portable solution for representing relative pathnames,
But unless you are running on a Unix system, it is not a general solution
to representing native pathnames.

An error is signaled if the argument is not NULL, a STRING or a PATHNAME,
or if it is a PATHNAME but some of its components are not recognized."
  (etypecase pathname
    ((or null string) pathname)
    (pathname
     (with-output-to-string (s)
       (flet ((err () (error "Not a valid unix-namestring ~S" pathname)))
         (let* ((dir (normalize-pathname-directory-component (pathname-directory pathname)))
                (name (pathname-name pathname))
                (type (pathname-type pathname))
                (type (and (not (eq type :unspecific)) type)))
           (cond
             ((eq dir ()))
             ((eq dir '(:relative)) (princ "./" s))
             ((consp dir)
              (destructuring-bind (relabs &rest dirs) dir
                (or (member relabs '(:relative :absolute)) (err))
                (when (eq relabs :absolute) (princ #\/ s))
                (loop :for x :in dirs :do
                  (cond
                    ((member x '(:back :up)) (princ "../" s))
                    ((equal x "") (err))
                    ;;((member x '("." "..") :test 'equal) (err))
                    ((stringp x) (format s "~A/" x))
                    (t (err))))))
             (t (err)))
           (cond
             (name
              (or (and (stringp name) (or (null type) (stringp type))) (err))
              (format s "~A~@[.~A~]" name type))
             (t
              (or (null type) (err))))))))))

(defun* subpathname (pathname subpath &key type)
  "This function takes a PATHNAME and a SUBPATH and a TYPE.
If SUBPATH is already a PATHNAME object (not namestring),
and is an absolute pathname at that, it is returned unchanged;
otherwise, SUBPATH is turned into a relative pathname with given TYPE
as per PARSE-UNIX-NAMESTRING with :WANT-RELATIVE T :TYPE TYPE,
then it is merged with the PATHNAME-DIRECTORY-PATHNAME of PATHNAME."
  (or (and (pathnamep subpath) (absolute-pathname-p subpath))
      (merge-pathnames* (parse-unix-namestring subpath :type type :want-relative t)
                        (pathname-directory-pathname pathname))))

(defun* subpathname* (pathname subpath &key type)
  "returns NIL if the base pathname is NIL, otherwise like SUBPATHNAME."
  (and pathname
       (subpathname (ensure-directory-pathname pathname) subpath :type type)))

;;; Pathname host and its root
(defun* pathname-root (pathname)
  (make-pathname* :directory '(:absolute)
                  :name nil :type nil :version nil
                  :defaults pathname ;; host device, and on scl, *some*
                  ;; scheme-specific parts: port username password, not others:
                  . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

(defun* pathname-host-pathname (pathname)
  (make-pathname* :directory nil
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
        (split-unix-namestring-directory-components root-string :ensure-directory t)
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

(defun* subpathp (maybe-subpath base-pathname)
  (and (pathnamep maybe-subpath) (pathnamep base-pathname)
       (absolute-pathname-p maybe-subpath) (absolute-pathname-p base-pathname)
       (directory-pathname-p base-pathname) (not (wild-pathname-p base-pathname))
       (pathname-equal (pathname-root maybe-subpath) (pathname-root base-pathname))
       (with-pathname-defaults ()
         (let ((enough (enough-namestring maybe-subpath base-pathname)))
           (and (relative-pathname-p enough) (pathname enough))))))


;;; Resolving symlinks somewhat
(defun* truenamize (pathname &optional (defaults *default-pathname-defaults*))
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep pathname '(or null logical-pathname)) (return pathname))
    (let ((p (merge-pathnames* pathname defaults)))
      (when (logical-pathname-p p) (return p))
      (let ((found (probe-file* p :truename t)))
        (when found (return found)))
      (unless (absolute-pathname-p p)
        (let ((true-defaults (truename* defaults)))
          (when true-defaults
            (setf p (merge-pathnames pathname true-defaults)))))
      (unless (absolute-pathname-p p) (return p))
      (let ((sofar (probe-file* (pathname-root p) :truename t)))
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
            :for dir :in (cdr directory)
            :for rest :on (cdr directory)
            :for more = (probe-file*
                         (merge-pathnames*
                          (make-pathname* :directory `(:relative ,dir))
                          sofar) :truename t) :do
            (if more
                (setf sofar more)
                (return (solution rest)))
            :finally
            (return (solution nil))))))))

(defun* resolve-symlinks (path)
  #-allegro (truenamize path)
  #+allegro
  (if (physical-pathname-p path)
      (or (ignore-errors (excl:pathname-resolve-symbolic-links path)) path)
      path))

(defun* resolve-symlinks* (path)
  (if *resolve-symlinks*
      (and path (resolve-symlinks path))
      path))


;;; absolute vs relative
(defun* ensure-pathname-absolute (path &optional defaults (on-error 'error))
  (cond
    ((absolute-pathname-p path))
    ((stringp path) (ensure-pathname-absolute (pathname path) defaults))
    ((not (pathnamep path)) (call-function on-error "not a valid pathname designator ~S" path))
    ((absolute-pathname-p defaults)
     (or (absolute-pathname-p (merge-pathnames* path defaults))
         (call-function on-error "Failed to merge ~S with ~S into an absolute pathname"
                        path defaults)))
    (t (call-function on-error
                      "Cannot ensure ~S is evaluated as an absolute pathname with defaults ~S"
                      path defaults))))

(defun relativize-directory-component (directory-component)
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
  (when (probe-file* x)
    (delete-file x)))

;;; Translate a pathname
(defun* (translate-pathname*) (path absolute-source destination &optional root source)
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
      (delete-file-if-exists staging))))

(defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
  `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body)))

;;; Basic pathnames
(defun* sane-physical-pathname (&key defaults (keep t) fallback want-existing)
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
             (when want-existing ;; CCL's probe-file will choke if d-p-d is logical
               (setf x (probe-file* x)))
             (and (physical-pathname-p x) x))))
    (or (sanitize defaults)
        (when fallback
          (or (sanitize (nil-pathname))
              (sanitize (ignore-errors (user-homedir-pathname)))))
        (error "Could not find a sane a physical pathname~
                ~@[ from ~S~]~@[~:*~@[ or~*~] fallbacks~]"
               defaults fallback))))

(defun* root-pathname ()
  "On a Unix system, this will presumably be the root pathname /.
Otherwise, this will be the root of some implementation-dependent filesystem host."
  (sane-physical-pathname :keep :root :fallback t))


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


;;; Check pathname constraints

(defun* ensure-pathname
    (pathname &key
              on-error
              defaults type dot-dot
              want-pathname
              want-logical want-physical ensure-physical
              want-relative want-absolute ensure-absolute ensure-subpath
              want-non-wild want-wild wilden
              want-file want-directory ensure-directory
              want-existing ensure-directories-exist
              truename resolve-symlinks truenamize
              &aux (p pathname)) ;; mutable working copy, preserve original
  "Coerces its argument into a PATHNAME,
optionally doing some transformations and checking specified constraints.

If the argument is NIL, then NIL is returned unless the WANT-PATHNAME constraint is specified.

If the argument is a STRING, it is first converted to a pathname via PARSE-UNIX-NAMESTRING
reusing the keywords DEFAULTS TYPE DOT-DOT ENSURE-DIRECTORY WANT-RELATIVE;
then the result is optionally merged into the DEFAULTS if ENSURE-ABSOLUTE is true,
and the all the checks and transformations are run.

Each non-nil constraint argument can be one of the symbols T, ERROR, CERROR or IGNORE.
The boolean T is an alias for ERROR.
ERROR means that an error will be raised if the constraint is not satisfied.
CERROR means that an continuable error will be raised if the constraint is not satisfied.
IGNORE means just return NIL instead of the pathname.

The ON-ERROR argument, if not NIL, is a function designator (as per CALL-FUNCTION)
that will be called with the the following arguments:
a generic format string for ensure pathname, the pathname,
the keyword argument corresponding to the failed check or transformation,
a format string for the reason ENSURE-PATHNAME failed,
and a list with arguments to that format string.
If ON-ERROR is NIL, ERROR is used instead, which does the right thing.
You could also pass (CERROR \"CONTINUE DESPITE FAILED CHECK\").

The transformations and constraint checks are done in this order,
which is also the order in the lambda-list:

WANT-PATHNAME checks that pathname (after parsing if needed) is not null.
Otherwise, if the pathname is NIL, ensure-pathname returns NIL.
WANT-LOGICAL checks that pathname is a LOGICAL-PATHNAME
WANT-PHYSICAL checks that pathname is not a LOGICAL-PATHNAME
ENSURE-PHYSICAL ensures that pathname is physical via TRANSLATE-LOGICAL-PATHNAME
WANT-RELATIVE checks that pathname has a relative directory component
WANT-ABSOLUTE checks that pathname does have an absolute directory component
ENSURE-ABSOLUTE merges with the DEFAULTS, then checks again
that the result absolute is an absolute pathname indeed.
ENSURE-SUBPATH checks that the pathname is a subpath of the DEFAULTS.
WANT-FILE checks that pathname has a non-nil FILE component
WANT-DIRECTORY checks that pathname has nil FILE and TYPE components
ENSURE-DIRECTORY uses ENSURE-DIRECTORY-PATHNAME to interpret
any file and type components as being actually a last directory component.
WANT-NON-WILD checks that pathname is not a wild pathname
WANT-WILD checks that pathname is a wild pathname
WILDEN merges the pathname with **/*.*.* if it is not wild
WANT-EXISTING checks that a file (or directory) exists with that pathname.
ENSURE-DIRECTORIES-EXIST creates any parent directory with ENSURE-DIRECTORIES-EXIST.
TRUENAME replaces the pathname by its truename, or errors if not possible.
RESOLVE-SYMLINKS replaces the pathname by a variant with symlinks resolved by RESOLVE-SYMLINKS.
TRUENAMIZE uses TRUENAMIZE to resolve as many symlinks as possible."
  (block nil
    (flet ((report-error (keyword description &rest arguments)
             (call-function (or on-error 'error)
                            "Invalid pathname ~S: ~*~?"
                            pathname keyword description arguments)))
      (macrolet ((err (constraint &rest arguments)
                   `(report-error ',(intern* constraint :keyword) ,@arguments))
                 (check (constraint condition &rest arguments)
                   `(when ,constraint
                      (unless ,condition (err ,constraint ,@arguments))))
                 (transform (transform condition expr)
                   `(when ,transform
                      (,@(if condition `(when ,condition) '(progn))
                       (setf p ,expr)))))
        (etypecase p
          ((or null pathname))
          (string
           (setf p (parse-unix-namestring
                    p :defaults defaults :type type :dot-dot dot-dot
                    :ensure-directory ensure-directory :want-relative want-relative))))
        (check want-pathname (pathnamep p) "Expected a pathname, not NIL")
        (unless (pathnamep p) (return nil))
        (check want-logical (logical-pathname-p p) "Expected a logical pathname")
        (check want-physical (physical-pathname-p p) "Expected a physical pathname")
        (transform ensure-physical () (translate-logical-pathname p))
        (check ensure-physical (physical-pathname-p p) "Could not translate to a physical pathname")
        (check want-relative (relative-pathname-p p) "Expected a relative pathname")
        (check want-absolute (absolute-pathname-p p) "Expected an absolute pathname")
        (transform ensure-absolute (not (absolute-pathname-p p)) (merge-pathnames* p defaults))
        (check ensure-absolute (absolute-pathname-p p)
               "Could not make into an absolute pathname even after merging with ~S" defaults)
        (check ensure-subpath (absolute-pathname-p defaults)
               "cannot be checked to be a subpath of non-absolute pathname ~S" defaults)
        (check ensure-subpath (subpathp p defaults) "is not a sub pathname of ~S" defaults)
        (check want-file (file-pathname-p p) "Expected a file pathname")
        (check want-directory (directory-pathname-p p) "Expected a directory pathname")
        (transform ensure-directory (not (directory-pathname-p p)) (ensure-directory-pathname p))
        (check want-non-wild (not (wild-pathname-p p)) "Expected a non-wildcard pathname")
        (check want-wild (wild-pathname-p p) "Expected a wildcard pathname")
        (transform wilden (not (wild-pathname-p p)) (wilden p))
        (when want-existing
          (let ((existing (probe-file* p :truename truename)))
            (if existing
                (when truename
                  (return existing))
                (err want-existing "Expected an existing pathname"))))
        (when ensure-directories-exist (ensure-directories-exist p))
        (when truename
          (let ((truename (truename* p)))
            (if truename
                (return truename)
                (err truename "Can't get a truename for pathname"))))
        (transform resolve-symlinks () (resolve-symlinks p))
        (transform truenamize () (truenamize p))
        p))))


(defun absolutize-pathnames
    (pathnames &key type (resolve-symlinks *resolve-symlinks*) truename)
    "Given a list of PATHNAMES where each is in the context of the next ones,
try to resolve these pathnames into an absolute pathname; first gently, then harder."
  (block nil
    (labels ((resolve (x)
               (or (when truename
                     (absolute-pathname-p (truename* x)))
                   (when resolve-symlinks
                     (absolute-pathname-p (resolve-symlinks x)))
                   (absolute-pathname-p x)
                   (unless resolve-symlinks
                     (absolute-pathname-p (resolve-symlinks x)))
                   (unless truename
                     (absolute-pathname-p (truename* x)))
                   (return nil)))
             (tryone (x type rest)
               (resolve (or (absolute-pathname-p x)
                            (subpathname (recurse rest :directory) x :type type))))
             (recurse (pathnames type)
               (if (null pathnames) (return nil)
                   (tryone (first pathnames) type (rest pathnames)))))
      (recurse pathnames type))))


;;; Hook for output translations
(defvar *output-translation-function* 'identity)
