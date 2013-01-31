;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp filesystem access

(asdf/package:define-package :asdf/filesystem
  (:recycle :asdf/pathname :asdf)
  (:use :asdf/common-lisp :asdf/package :asdf/utility :asdf/os :asdf/pathname)
  (:export
   ;; Native namestrings
   #:native-namestring #:parse-native-namestring
   ;; Probing the filesystem
   #:truename* #:safe-file-write-date #:probe-file*
   #:directory* #:filter-logical-directory-results #:directory-files #:subdirectories
   #:collect-sub*directories
   ;; Resolving symlinks somewhat
   #:truenamize #:resolve-symlinks #:*resolve-symlinks* #:resolve-symlinks*
   ;; merging with cwd
   #:get-pathname-defaults #:call-with-current-directory #:with-current-directory
   ;; Environment pathnames
   #:inter-directory-separator #:split-native-pathnames-string
   #:getenv-pathname #:getenv-pathnames
   #:getenv-absolute-directory #:getenv-absolute-directories
   #:lisp-implementation-directory #:lisp-implementation-pathname-p
   ;; Simple filesystem operations
   #:ensure-all-directories-exist
   #:rename-file-overwriting-target
   #:delete-file-if-exists))
(in-package :asdf/filesystem)

;;; Native namestrings, as seen by the operating system calls rather than Lisp
(defun* native-namestring (x)
  "From a non-wildcard CL pathname, a return namestring suitable for passing to the operating system"
  (when x
    (let ((p (pathname x)))
      #+clozure (with-pathname-defaults () (ccl:native-translated-namestring p)) ; see ccl bug 978
      #+(or cmu scl) (ext:unix-namestring p nil)
      #+sbcl (sb-ext:native-namestring p)
      #-(or clozure cmu sbcl scl)
      (if (os-unix-p) (unix-namestring p)
          (namestring p)))))

(defun* parse-native-namestring (string &rest constraints &key ensure-directory &allow-other-keys)
  "From a native namestring suitable for use by the operating system, return
a CL pathname satisfying all the specified constraints as per ENSURE-PATHNAME"
  (check-type string (or string null))
  (let* ((pathname
           (when string
             (with-pathname-defaults ()
               #+clozure (ccl:native-to-pathname string)
               #+sbcl (sb-ext:parse-native-namestring string)
               #-(or clozure sbcl)
               (if (os-unix-p)
                   (parse-unix-namestring string :ensure-directory ensure-directory)
                   (parse-namestring string)))))
         (pathname
           (if ensure-directory
               (and pathname (ensure-directory-pathname pathname))
               pathname)))
    (apply 'ensure-pathname pathname constraints)))


;;; Probing the filesystem
(defun* truename* (p)
  ;; avoids both logical-pathname merging and physical resolution issues
  (and p (handler-case (with-pathname-defaults () (truename p)) (file-error () nil))))

(defun* safe-file-write-date (pathname)
  ;; If FILE-WRITE-DATE returns NIL, it's possible that
  ;; the user or some other agent has deleted an input file.
  ;; Also, generated files will not exist at the time planning is done
  ;; and calls compute-action-stamp which calls safe-file-write-date.
  ;; So it is very possible that we can't get a valid file-write-date,
  ;; and we can survive and we will continue the planning
  ;; as if the file were very old.
  ;; (or should we treat the case in a different, special way?)
  (handler-case (file-write-date (translate-logical-pathname pathname)) (file-error () nil)))

(defun* probe-file* (p &key truename)
  "when given a pathname P (designated by a string as per PARSE-NAMESTRING),
probes the filesystem for a file or directory with given pathname.
If it exists, return its truename is ENSURE-PATHNAME is true,
or the original (parsed) pathname if it is false (the default)."
  (with-pathname-defaults () ;; avoids logical-pathname issues on some implementations
    (etypecase p
      (null nil)
      (string (probe-file* (parse-namestring p) :truename truename))
      (pathname
       (handler-case
           (or
            #+allegro
            (probe-file p :follow-symlinks truename)
            #-(or allegro clisp gcl2.6)
            (if truename
                (probe-file p)
                (and (ignore-errors
                      (let ((pp (translate-logical-pathname p)))
                        #+(or cmu scl) (unix:unix-stat (ext:unix-namestring pp))
                        #+(and lispworks unix) (system:get-file-stat pp)
                        #+sbcl (sb-unix:unix-stat (sb-ext:native-namestring pp))
                        #-(or cmu (and lispworks unix) sbcl scl) (file-write-date pp)))
                     p))
            #+(or clisp gcl2.6)
            #.(flet ((probe (probe)
                       `(let ((foundtrue ,probe))
                          (cond
                            (truename foundtrue)
                            (foundtrue p)))))
                #+gcl2.6
                (probe '(or (probe-file p)
                         (and (directory-pathname-p p)
                          (ignore-errors
                           (ensure-directory-pathname
                            (truename* (subpathname
                                        (ensure-directory-pathname p) ".")))))))
                #+clisp
                (let* ((fs (find-symbol* '#:file-stat :posix nil))
                       (pp (find-symbol* '#:probe-pathname :ext nil))
                       (resolve (if pp
                                    `(ignore-errors (,pp p))
                                    '(or (truename* p)
                                      (truename* (ignore-errors (ensure-directory-pathname p)))))))
                  (if fs
                      `(if truename
                           ,resolve
                           (and (ignore-errors (,fs p)) p))
                      (probe resolve)))))
         (file-error () nil))))))

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

;;; Resolving symlinks somewhat
(defun* truenamize (pathname)
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep pathname '(or null logical-pathname)) (return pathname))
    (let ((p pathname))
      (unless (absolute-pathname-p p)
        (setf p (or (absolute-pathname-p (ensure-absolute-pathname p 'get-pathname-defaults nil))
                    (return p))))
      (when (logical-pathname-p p) (return p))
      (let ((found (probe-file* p :truename t)))
        (when found (return found)))
      (let* ((directory (normalize-pathname-directory-component (pathname-directory p)))
             (up-components (reverse (rest directory)))
             (down-components ()))
        (assert (eq :absolute (first directory)))
        (loop :while up-components :do
          (if-let (parent (probe-file* (make-pathname* :directory `(:absolute ,@(reverse up-components))
                                                       :name nil :type nil :version nil :defaults p)))
            (return (merge-pathnames* (make-pathname* :directory `(:relative ,@down-components)
                                                      :defaults p)
                                      (ensure-directory-pathname parent)))
            (push (pop up-components) down-components))
          :finally (return p))))))

(defun* resolve-symlinks (path)
  #-allegro (truenamize path)
  #+allegro
  (if (physical-pathname-p path)
      (or (ignore-errors (excl:pathname-resolve-symbolic-links path)) path)
      path))

(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.
Defaults to T.")

(defun* resolve-symlinks* (path)
  (if *resolve-symlinks*
      (and path (resolve-symlinks path))
      path))


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


;;; Pathname defaults
(defun* get-pathname-defaults (&optional (defaults *default-pathname-defaults*))
  (or (absolute-pathname-p defaults)
      (merge-pathnames* defaults (getcwd))))

(defun* call-with-current-directory (dir thunk)
  (if dir
      (let* ((dir (resolve-symlinks* (get-pathname-defaults (pathname-directory-pathname dir))))
             (*default-pathname-defaults* dir)
             (cwd (getcwd)))
        (chdir dir)
        (unwind-protect
             (funcall thunk)
          (chdir cwd)))
      (funcall thunk)))

(defmacro with-current-directory ((&optional dir) &body body)
  "Call BODY while the POSIX current working directory is set to DIR"
  `(call-with-current-directory ,dir #'(lambda () ,@body)))


;;; Environment pathnames
(defun* inter-directory-separator ()
  (if (os-unix-p) #\: #\;))

(defun* split-native-pathnames-string (string &rest constraints &key &allow-other-keys)
  (loop :for namestring :in (split-string string :separator (string (inter-directory-separator)))
        :collect (apply 'parse-native-namestring namestring constraints)))

(defun* getenv-pathname (x &rest constraints &key on-error &allow-other-keys)
  (apply 'parse-native-namestring (getenvp x)
         :on-error (or on-error
                       `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathname ,x))
         constraints))
(defun* getenv-pathnames (x &rest constraints &key on-error &allow-other-keys)
  (apply 'split-native-pathnames-string (getenvp x)
         :on-error (or on-error
                       `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathnames ,x))
         constraints))
(defun* getenv-absolute-directory (x)
  (getenv-pathname x :want-absolute t :ensure-directory t))
(defun* getenv-absolute-directories (x)
  (getenv-pathnames x :want-absolute t :ensure-directory t))

(defun* lisp-implementation-directory (&key truename)
  (declare (ignorable truename))
  #+(or clozure ecl gcl mkcl sbcl)
  (let ((dir
          (ignore-errors
           #+clozure #p"ccl:"
           #+(or ecl mkcl) #p"SYS:"
           #+gcl system::*system-directory*
           #+sbcl (if-let (it (find-symbol* :sbcl-homedir-pathname :sb-int nil))
                     (funcall it)
                     (getenv-pathname "SBCL_HOME" :ensure-directory t)))))
    (if (and dir truename)
        (truename* dir)
        dir)))

(defun* lisp-implementation-pathname-p (pathname)
  ;; Other builtin systems are those under the implementation directory
  (and (when pathname
         (if-let (impdir (lisp-implementation-directory))
           (or (subpathp pathname impdir)
               (when *resolve-symlinks*
                 (if-let (truename (truename* pathname))
                   (if-let (trueimpdir (truename* impdir))
                     (subpathp truename trueimpdir)))))))
       t))


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
  (handler-case (delete-file x) (file-error () nil)))


