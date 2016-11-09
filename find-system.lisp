;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/find-system
  (:recycle :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
    :asdf/cache :asdf/component :asdf/system)
  (:export
   #:remove-entry-from-registry #:coerce-entry-to-directory
   #:coerce-name #:primary-system-name #:coerce-filename
   #:find-system #:locate-system #:load-asd
   #:system-registered-p #:registered-system #:register-system
   #:registered-systems* #:registered-systems
   #:clear-system #:map-systems
   #:missing-component #:missing-requires #:missing-parent
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:find-system-if-being-defined
   #:contrib-sysdef-search #:sysdef-find-asdf ;; backward compatibility symbols, functions removed
   #:sysdef-preloaded-system-search #:register-preloaded-system #:*preloaded-systems*
   #:mark-component-preloaded ;; forward reference to asdf/operate
   #:sysdef-immutable-system-search #:register-immutable-system #:*immutable-systems*
   #:*defined-systems* #:clear-defined-systems
   ;; defined in source-registry, but specially mentioned here:
   #:initialize-source-registry #:sysdef-source-registry-search))
(in-package :asdf/find-system)

(with-upgradability ()
  (declaim (ftype (function (&optional t) t) initialize-source-registry)) ; forward reference

  (define-condition missing-component (system-definition-error)
    ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
     (parent :initform nil :reader missing-parent :initarg :parent)))

  (define-condition formatted-system-definition-error (system-definition-error)
    ((format-control :initarg :format-control :reader format-control)
     (format-arguments :initarg :format-arguments :reader format-arguments))
    (:report (lambda (c s)
               (apply 'format s (format-control c) (format-arguments c)))))

  (define-condition load-system-definition-error (system-definition-error)
    ((name :initarg :name :reader error-name)
     (pathname :initarg :pathname :reader error-pathname)
     (condition :initarg :condition :reader error-condition))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while trying to load definition for system ~A from pathname ~A: ~3i~_~A~@:>")
                       (error-name c) (error-pathname c) (error-condition c)))))

  (defun sysdef-error (format &rest arguments)
    (error 'formatted-system-definition-error :format-control
           format :format-arguments arguments))


  ;;; Canonicalizing system names

  (defun coerce-name (name)
    "Given a designator for a component NAME, return the name as a string.
The designator can be a COMPONENT (designing its name; note that a SYSTEM is a component),
a SYMBOL (designing its name, downcased), or a STRING (designing itself)."
    (typecase name
      (component (component-name name))
      (symbol (string-downcase name))
      (string name)
      (t (sysdef-error (compatfmt "~@<Invalid component designator: ~3i~_~A~@:>") name))))

  (defun primary-system-name (name)
    "Given a system designator NAME, return the name of the corresponding primary system,
after which the .asd file is named. That's the first component when dividing the name
as a string by / slashes."
    (first (split-string (coerce-name name) :separator "/")))

  (defun coerce-filename (name)
    "Coerce a system designator NAME into a string suitable as a filename component.
The (current) transformation is to replace characters /:\\ each by --,
the former being forbidden in a filename component.
NB: The onus is unhappily on the user to avoid clashes."
    (frob-substrings (coerce-name name) '("/" ":" "\\") "--"))


  ;;; Registry of Defined Systems

  (defvar *defined-systems* (make-hash-table :test 'equal)
    "This is a hash table whose keys are strings -- the
names of systems -- and whose values are pairs, the first
element of which is a universal-time indicating when the
system definition was last updated, and the second element
of which is a system object.
  A system is referred to as \"registered\" if it is present
in this table.")

  (defun system-registered-p (name)
    "Return a generalized boolean that is true if a system of given NAME was registered already.
NAME is a system designator, to be normalized by COERCE-NAME.
The value returned if true is a pair of a timestamp and a system object."
    (gethash (coerce-name name) *defined-systems*))

  (defun registered-system (name)
    "Return a system of given NAME that was registered already,
if such a system exists.  NAME is a system designator, to be
normalized by COERCE-NAME. The value returned is a system object,
or NIL if not found."
    (cdr (system-registered-p name)))

  (defun registered-systems* ()
    "Return a list containing every registered system (as a system object)."
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :collect (cdr registered)))

  (defun registered-systems ()
    "Return a list of the names of every registered system."
    (mapcar 'coerce-name (registered-systems*)))

  (defun register-system (system)
    "Given a SYSTEM object, register it."
    (check-type system system)
    (let ((name (component-name system)))
      (check-type name string)
      (asdf-message (compatfmt "~&~@<; ~@;Registering ~3i~_~A~@:>~%") system)
      (unless (eq system (registered-system name))
        (setf (gethash name *defined-systems*)
              (cons (ignore-errors (get-file-stamp (system-source-file system)))
                    system)))))

  (defun map-systems (fn)
    "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :do (funcall fn (cdr registered))))


  ;;; Preloaded systems: in the image even if you can't find source files backing them.

  (defvar *preloaded-systems* (make-hash-table :test 'equal)
    "Registration table for preloaded systems.")

  (declaim (ftype (function (t) t) mark-component-preloaded)) ; defined in asdf/operate

  (defun make-preloaded-system (name keys)
    "Make a preloaded system of given NAME with build information from KEYS"
    (let ((system (apply 'make-instance (getf keys :class 'system)
                         :name name :source-file (getf keys :source-file)
                         (remove-plist-keys '(:class :name :source-file) keys))))
      (mark-component-preloaded system)
      system))

  (defun sysdef-preloaded-system-search (requested)
    "If REQUESTED names a system registered as preloaded, return a new system
with its registration information."
    (let ((name (coerce-name requested)))
      (multiple-value-bind (keys foundp) (gethash name *preloaded-systems*)
        (when foundp
          (make-preloaded-system name keys)))))

  (defun ensure-preloaded-system-registered (name)
    "If there isn't a registered _defined_ system of given NAME,
and a there is a registered _preloaded_ system of given NAME,
then define and register said preloaded system."
    (if-let (system (and (not (registered-system name)) (sysdef-preloaded-system-search name)))
      (register-system system)))

  (defun register-preloaded-system (system-name &rest keys &key (version t) &allow-other-keys)
    "Register a system as being preloaded. If the system has not been loaded from the filesystem
yet, or if its build information is later cleared with CLEAR-SYSTEM, a dummy system will be
registered without backing filesystem information, based on KEYS (e.g. to provide a VERSION).
If VERSION is the default T, and a system was already loaded, then its version will be preserved."
    (let ((name (coerce-name system-name)))
      (when (eql version t)
        (if-let (system (registered-system name))
          (setf (getf keys :version) (component-version system))))
      (setf (gethash name *preloaded-systems*) keys)
      (ensure-preloaded-system-registered system-name)))


  ;;; Immutable systems: in the image and can't be reloaded from source.

  (defvar *immutable-systems* nil
    "A hash-set (equal hash-table mapping keys to T) of systems that are immutable,
i.e. already loaded in memory and not to be refreshed from the filesystem.
They will be treated specially by find-system, and passed as :force-not argument to make-plan.

For instance, to can deliver an image with many systems precompiled, that *will not* check the
filesystem for them every time a user loads an extension, what more risk a problematic upgrade
 or catastrophic downgrade, before you dump an image, you may use:
   (map () 'asdf:register-immutable-system (asdf:already-loaded-systems))

Note that direct access to this variable from outside ASDF is not supported.
Please call REGISTER-IMMUTABLE-SYSTEM to add new immutable systems, and
contact maintainers if you need a stable API to do more than that.")

  (defun sysdef-immutable-system-search (requested)
    (let ((name (coerce-name requested)))
      (when (and *immutable-systems* (gethash name *immutable-systems*))
        (or (registered-system requested)
            (error 'formatted-system-definition-error
                   :format-control "Requested system ~A registered as an immutable-system, ~
but not even registered as defined"
                   :format-arguments (list name))))))

  (defun register-immutable-system (system-name &rest keys)
    "Register SYSTEM-NAME as preloaded and immutable.
It will automatically be considered as passed to FORCE-NOT in a plan."
    (let ((system-name (coerce-name system-name)))
      (apply 'register-preloaded-system system-name keys)
      (unless *immutable-systems*
        (setf *immutable-systems* (list-to-hash-set nil)))
      (setf (gethash system-name *immutable-systems*) t)))


  ;;; Making systems undefined.

  (defun clear-system (system)
    "Clear the entry for a SYSTEM in the database of systems previously defined.
However if the system was registered as PRELOADED (which it is if it is IMMUTABLE),
then a new system with the same name will be defined and registered in its place
from which build details will have been cleared.
Note that this does NOT in any way cause any of the code of the system to be unloaded.
Returns T if system was or is now undefined, NIL if a new preloaded system was redefined."
    ;; There is no "unload" operation in Common Lisp, and
    ;; a general such operation cannot be portably written,
    ;; considering how much CL relies on side-effects to global data structures.
    (let ((name (coerce-name system)))
      (remhash name *defined-systems*)
      (unset-asdf-cache-entry `(find-system ,name))
      (not (ensure-preloaded-system-registered name))))

  (defun clear-defined-systems ()
    "Clear all currently registered defined systems.
Preloaded systems (including immutable ones) will be reset, other systems will be de-registered."
    (loop :for name :being :the :hash-keys :of *defined-systems*
          :unless (member name '("asdf" "uiop") :test 'equal) :do (clear-system name)))


  ;;; Searching for system definitions

  ;; For the sake of keeping things reasonably neat, we adopt a convention that
  ;; only symbols are to be pushed to this list (rather than e.g. function objects),
  ;; which makes upgrade easier. Also, the name of these symbols shall start with SYSDEF-
  (defvar *system-definition-search-functions* '()
    "A list that controls the ways that ASDF looks for system definitions.
It contains symbols to be funcalled in order, with a requested system name as argument,
until one returns a non-NIL result (if any), which must then be a fully initialized system object
with that name.")

  ;; Initialize and/or upgrade the *system-definition-search-functions*
  ;; so it doesn't contain obsolete symbols, and does contain the current ones.
  (defun cleanup-system-definition-search-functions ()
    (setf *system-definition-search-functions*
          (append
           ;; Remove known-incompatible sysdef functions from old versions of asdf.
           ;; Order matters, so we can't just use set-difference.
           (let ((obsolete
                  '(contrib-sysdef-search sysdef-find-asdf sysdef-preloaded-system-search)))
             (remove-if #'(lambda (x) (member x obsolete)) *system-definition-search-functions*))
           ;; Tuck our defaults at the end of the list if they were absent.
           ;; This is imperfect, in case they were removed on purpose,
           ;; but then it will be the responsibility of whoever removes these symmbols
           ;; to upgrade asdf before he does such a thing rather than after.
           (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                      '(sysdef-central-registry-search
                        sysdef-source-registry-search)))))
  (cleanup-system-definition-search-functions)

  ;; This (private) function does the search for a system definition using *s-d-s-f*;
  ;; it is to be called by locate-system.
  (defun search-for-system-definition (system)
    ;; Search for valid definitions of the system available in the current session.
    ;; Previous definitions as registered in *defined-systems* MUST NOT be considered;
    ;; they will be reconciled by locate-system then find-system.
    ;; There are two special treatments: first, specially search for objects being defined
    ;; in the current session, to avoid definition races between several files;
    ;; second, specially search for immutable systems, so they cannot be redefined.
    ;; Finally, use the search functions specified in *system-definition-search-functions*.
    (let ((name (coerce-name system)))
      (flet ((try (f) (if-let ((x (funcall f name))) (return-from search-for-system-definition x))))
        (try 'find-system-if-being-defined)
        (try 'sysdef-immutable-system-search)
        (map () #'try *system-definition-search-functions*))))


  ;;; The legacy way of finding a system: the *central-registry*

  ;; This variable contains a list of directories to be lazily searched for the requested asd
  ;; by sysdef-central-registry-search.
  (defvar *central-registry* nil
    "A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This variable is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.")

  ;; Function to look for an asd file of given NAME under a directory provided by DEFAULTS.
  ;; Return the truename of that file if it is found and TRUENAME is true.
  ;; Return NIL if the file is not found.
  ;; On Windows, follow shortcuts to .asd files.
  (defun probe-asd (name defaults &key truename)
    (block nil
      (when (directory-pathname-p defaults)
        (if-let (file (probe-file*
                       (ensure-absolute-pathname
                        (parse-unix-namestring name :type "asd")
                        #'(lambda () (ensure-absolute-pathname defaults 'get-pathname-defaults nil))
                        nil)
                       :truename truename))
          (return file))
        #-(or clisp genera) ; clisp doesn't need it, plain genera doesn't have read-sequence(!)
        (os-cond
         ((os-windows-p)
          (when (physical-pathname-p defaults)
            (let ((shortcut
                    (make-pathname
                     :defaults defaults :case :local
                     :name (strcat name ".asd")
                     :type "lnk")))
              (when (probe-file* shortcut)
                (ensure-pathname (parse-windows-shortcut shortcut) :namestring :native)))))))))

  ;; Function to push onto *s-d-s-f* to use the *central-registry*
  (defun sysdef-central-registry-search (system)
    (let ((name (primary-system-name system))
          (to-remove nil)
          (to-replace nil))
      (block nil
        (unwind-protect
             (dolist (dir *central-registry*)
               (let ((defaults (eval dir))
                     directorized)
                 (when defaults
                   (cond ((directory-pathname-p defaults)
                          (let* ((file (probe-asd name defaults :truename *resolve-symlinks*)))
                            (when file
                              (return file))))
                         (t
                          (restart-case
                              (let* ((*print-circle* nil)
                                     (message
                                       (format nil
                                               (compatfmt "~@<While searching for system ~S: ~3i~_~S evaluated to ~S which is not an absolute directory.~@:>")
                                               system dir defaults)))
                                (error message))
                            (remove-entry-from-registry ()
                              :report "Remove entry from *central-registry* and continue"
                              (push dir to-remove))
                            (coerce-entry-to-directory ()
                              :test (lambda (c) (declare (ignore c))
                                      (and (not (directory-pathname-p defaults))
                                           (directory-pathname-p
                                            (setf directorized
                                                  (ensure-directory-pathname defaults)))))
                              :report (lambda (s)
                                        (format s (compatfmt "~@<Coerce entry to ~a, replace ~a and continue.~@:>")
                                                directorized dir))
                              (push (cons dir directorized) to-replace))))))))
          ;; cleanup
          (dolist (dir to-remove)
            (setf *central-registry* (remove dir *central-registry*)))
          (dolist (pair to-replace)
            (let* ((current (car pair))
                   (new (cdr pair))
                   (position (position current *central-registry*)))
              (setf *central-registry*
                    (append (subseq *central-registry* 0 position)
                            (list new)
                            (subseq *central-registry* (1+ position))))))))))


  ;;; Methods for find-system

  ;; Reject NIL as a system designator.
  (defmethod find-system ((name null) &optional (error-p t))
    (when error-p
      (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

  ;; Default method for find-system: resolve the argument using COERCE-NAME.
  (defmethod find-system (name &optional (error-p t))
    (find-system (coerce-name name) error-p))

  (defun find-system-if-being-defined (name)
    ;; This function finds systems being defined *in the current ASDF session*, as embodied by
    ;; its session cache, even before they are fully defined and registered in *defined-systems*.
    ;; The purpose of this function is to prevent races between two files that might otherwise
    ;; try overwrite each other's system objects, resulting in infinite loops and stack overflow.
    ;; This function explicitly MUST NOT find definitions merely registered in previous sessions.
    ;; NB: this function depends on a corresponding side-effect in parse-defsystem;
    ;; the precise protocol between the two functions may change in the future (or not).
    (first (gethash `(find-system ,(coerce-name name)) *asdf-cache*)))

  (defun load-asd (pathname
                   &key name (external-format (encoding-external-format (detect-encoding pathname)))
                   &aux (readtable *readtable*) (print-pprint-dispatch *print-pprint-dispatch*))
    "Load system definitions from PATHNAME.
NAME if supplied is the name of a system expected to be defined in that file.

Do NOT try to load a .asd file directly with CL:LOAD. Always use ASDF:LOAD-ASD."
    (with-asdf-cache ()
      (with-standard-io-syntax
        (let ((*package* (find-package :asdf-user))
              ;; Note that our backward-compatible *readtable* is
              ;; a global readtable that gets globally side-effected. Ouch.
              ;; Same for the *print-pprint-dispatch* table.
              ;; We should do something about that for ASDF3 if possible, or else ASDF4.
              (*readtable* readtable)
              (*print-pprint-dispatch* print-pprint-dispatch)
              (*print-readably* nil)
              (*default-pathname-defaults*
                ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
                (pathname-directory-pathname (physicalize-pathname pathname))))
          (handler-bind
              (((and error (not missing-component))
                 #'(lambda (condition)
                     (error 'load-system-definition-error
                            :name name :pathname pathname :condition condition))))
            (asdf-message (compatfmt "~&~@<; ~@;Loading system definition~@[ for ~A~] from ~A~@:>~%")
                          name pathname)
            (load* pathname :external-format external-format))))))

  (defvar *old-asdf-systems* (make-hash-table :test 'equal))

  ;; (Private) function to check that a system that was found isn't an asdf downgrade.
  ;; Returns T if everything went right, NIL if the system was an ASDF of the same or older version,
  ;; that shall not be loaded. Also issue a warning if it was a strictly older version of ASDF.
  (defun check-not-old-asdf-system (name pathname)
    (or (not (equal name "asdf"))
        (null pathname)
        (let* ((version-pathname (subpathname pathname "version.lisp-expr"))
               (version (and (probe-file* version-pathname :truename nil)
                             (read-file-form version-pathname)))
               (old-version (asdf-version)))
          (cond
            ((version< old-version version) t) ;; newer version: good!
            ((equal old-version version) nil) ;; same version: don't load, but don't warn
            (t ;; old version: bad
             (ensure-gethash
              (list (namestring pathname) version) *old-asdf-systems*
              #'(lambda ()
                 (let ((old-pathname (system-source-file (registered-system "asdf"))))
                   (warn "~@<~
        You are using ASDF version ~A ~:[(probably from (require \"asdf\") ~
        or loaded by quicklisp)~;from ~:*~S~] and have an older version of ASDF ~
        ~:[(and older than 2.27 at that)~;~:*~A~] registered at ~S. ~
        Having an ASDF installed and registered is the normal way of configuring ASDF to upgrade itself, ~
        and having an old version registered is a configuration error. ~
        ASDF will ignore this configured system rather than downgrade itself. ~
        In the future, you may want to either: ~
        (a) upgrade this configured ASDF to a newer version, ~
        (b) install a newer ASDF and register it in front of the former in your configuration, or ~
        (c) uninstall or unregister this and any other old version of ASDF from your configuration. ~
        Note that the older ASDF might be registered implicitly through configuration inherited ~
        from your system installation, in which case you might have to specify ~
        :ignore-inherited-configuration in your in your ~~/.config/common-lisp/source-registry.conf ~
        or other source-registry configuration file, environment variable or lisp parameter. ~
        Indeed, a likely offender is an obsolete version of the cl-asdf debian or ubuntu package, ~
        that you might want to upgrade (if a recent enough version is available) ~
        or else remove altogether (since most implementations ship with a recent asdf); ~
        if you lack the system administration rights to upgrade or remove this package, ~
        then you might indeed want to either install and register a more recent version, ~
        or use :ignore-inherited-configuration to avoid registering the old one. ~
        Please consult ASDF documentation and/or experts.~@:>~%"
                         old-version old-pathname version pathname))))
             nil))))) ;; only issue the warning the first time, but always return nil

  (defun locate-system (name)
    "Given a system NAME designator, try to locate where to load the system from.
Returns five values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a SYSTEM object that may be REGISTER-SYSTEM'ed.
PATHNAME when not null is a path from which to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded SYSTEM object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded."
    (with-asdf-cache () ;; NB: We don't cache the results. We once used to, but it wasn't useful,
      ;; and keeping a negative cache was a bug (see lp#1335323), which required
      ;; explicit invalidation in clear-system and find-system (when unsucccessful).
      (let* ((name (coerce-name name))
             (in-memory (system-registered-p name)) ; load from disk if absent or newer on disk
             (previous (cdr in-memory))
             (previous (and (typep previous 'system) previous))
             (previous-time (car in-memory))
             (found (search-for-system-definition name))
             (found-system (and (typep found 'system) found))
             (pathname (ensure-pathname
                        (or (and (typep found '(or pathname string)) (pathname found))
                            (system-source-file found-system)
                            (system-source-file previous))
                        :want-absolute t :resolve-symlinks *resolve-symlinks*))
             (foundp (and (or found-system pathname previous) t)))
        (check-type found (or null pathname system))
        (unless (check-not-old-asdf-system name pathname)
          (check-type previous system) ;; asdf is preloaded, so there should be a previous one.
          (setf found-system nil pathname nil))
        (values foundp found-system pathname previous previous-time))))

  ;; Main method for find-system: first, make sure the computation is memoized in a session cache.
  ;; unless the system is immutable, use locate-system to find the primary system;
  ;; reconcile the finding (if any) with any previous definition (in a previous session,
  ;; preloaded, with a previous configuration, or before filesystem changes), and
  ;; load a found .asd if appropriate. Finally, update registration table and return results.
  (defmethod find-system ((name string) &optional (error-p t))
    (with-asdf-cache (:key `(find-system ,name))
      (let ((primary-name (primary-system-name name)))
        (unless (equal name primary-name)
          (find-system primary-name nil)))
      (or (and *immutable-systems* (gethash name *immutable-systems*) (registered-system name))
          (multiple-value-bind (foundp found-system pathname previous previous-time)
              (locate-system name)
            (assert (eq foundp (and (or found-system pathname previous) t)))
            (let ((previous-pathname (system-source-file previous))
                  (system (or previous found-system)))
              (when (and found-system (not previous))
                (register-system found-system))
              (when (and system pathname)
                (setf (system-source-file system) pathname))
              (when (and pathname
                         (let ((stamp (get-file-stamp pathname)))
                           (and stamp
                                (not (and previous
                                          (or (pathname-equal pathname previous-pathname)
                                              (and pathname previous-pathname
                                                   (pathname-equal
                                                    (physicalize-pathname pathname)
                                                    (physicalize-pathname previous-pathname))))
                                          (stamp<= stamp previous-time))))))
                ;; Only load when it's a pathname that is different or has newer content.
                (load-asd pathname :name name)))
            ;; Try again after having loaded from disk if needed
            (let ((in-memory (system-registered-p name)))
              (cond
                (in-memory
                 (when pathname
                   (setf (car in-memory) (get-file-stamp pathname)))
                 (cdr in-memory))
                (error-p
                 (error 'missing-component :requires name))
                (t
                 (return-from find-system nil)))))))))
