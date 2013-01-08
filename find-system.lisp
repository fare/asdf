;;;; -------------------------------------------------------------------------
;;;; Finding systems

(defpackage :asdf/find-system
  (:use :common-lisp :asdf/implementation :asdf/utility :asdf/upgrade :asdf/pathname :asdf/os
   :asdf/component :asdf/system)
  (:export
   #:coerce-name #:find-system #:locate-system #:load-sysdef #:with-system-definitions
   #:system-registered-p #:register-system #:registered-systems #:clear-system #:map-systems
   #:system-definition-error #:missing-component #:missing-requires #:missing-parent
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:make-temporary-package #:find-system-if-being-defined #:*systems-being-defined*
   #:find-system-fallback #:sysdef-find-asdf
   #:make-defined-systems-table #:*defined-systems*
   ;; defined in source-registry, but specially mentioned here:
   #:initialize-source-registry #:sysdef-source-registry-search))
(in-package :asdf/find-system)

(declaim (ftype (function (&optional t) *) initialize-source-registry)) ; forward reference

(define-condition system-definition-error (error) ()
  ;; [this use of :report should be redundant, but unfortunately it's not.
  ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
  ;; over print-object; this is always conditions::%print-condition for
  ;; condition objects, which in turn does inheritance of :report options at
  ;; run-time.  fortunately, inheritance means we only need this kludge here in
  ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
  #+cmu (:report print-object))

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

(defun* sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error :format-control
         format :format-arguments arguments))

(defun* make-defined-systems-table ()
  (make-hash-table :test 'equal))

(defvar *defined-systems* (make-defined-systems-table)
  "This is a hash table whose keys are strings, being the
names of the systems, and whose values are pairs, the first
element of which is a universal-time indicating when the
system definition was last updated, and the second element
of which is a system object.")

(defun* coerce-name (name)
  (typecase name
    (component (component-name name))
    (symbol (string-downcase (symbol-name name)))
    (string name)
    (t (sysdef-error (compatfmt "~@<Invalid component designator: ~3i~_~A~@:>") name))))

(defun* system-registered-p (name)
  (gethash (coerce-name name) *defined-systems*))

(defun* registered-systems ()
  (loop :for (() . system) :being :the :hash-values :of *defined-systems*
    :collect (coerce-name system)))

(defun* register-system (system)
  (check-type system system)
  (let ((name (component-name system)))
    (check-type name string)
    (asdf-message (compatfmt "~&~@<; ~@;Registering ~3i~_~A~@:>~%") system)
    (unless (eq system (cdr (gethash name *defined-systems*)))
      (setf (gethash name *defined-systems*)
            (cons (aif (ignore-errors (system-source-file system)) (safe-file-write-date it)) system)))))

(defun* clear-system (name)
  "Clear the entry for a system in the database of systems previously loaded.
Note that this does NOT in any way cause the code of the system to be unloaded."
  ;; There is no "unload" operation in Common Lisp, and
  ;; a general such operation cannot be portably written,
  ;; considering how much CL relies on side-effects to global data structures.
  (remhash (coerce-name name) *defined-systems*))

(defun* map-systems (fn)
  "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
  (loop :for (nil . system) :being :the hash-values :of *defined-systems*
        :do (funcall fn system)))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-

(defvar *system-definition-search-functions* '())

(setf *system-definition-search-functions*
      (append
       ;; Remove known-incompatible sysdef functions from ancient sbcl asdf.
       (remove 'contrib-sysdef-search *system-definition-search-functions*)
       ;; Tuck our defaults at the end of the list if they were absent.
       ;; This is imperfect, in case they were removed on purpose,
       ;; but then it will be the responsibility of whoever does that
       ;; to upgrade asdf before he does such a thing rather than after.
       (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                  '(sysdef-central-registry-search
                    sysdef-source-registry-search
                    sysdef-find-asdf))))

(defun* search-for-system-definition (system)
  (some (let ((name (coerce-name system))) #'(lambda (x) (funcall x name)))
        (cons 'find-system-if-being-defined
              *system-definition-search-functions*)))

(defvar *central-registry* nil
"A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This is for backward compatibilily.
Going forward, we recommend new users should be using the source-registry.
")

(defun* probe-asd (name defaults)
  (block nil
    (when (directory-pathname-p defaults)
      (let* ((file (probe-file* (subpathname defaults (strcat name ".asd")))))
        (when file
          (return file)))
      #-(or clisp genera) ; clisp doesn't need it, plain genera doesn't have read-sequence(!)
      (when (os-windows-p)
        (let ((shortcut
               (make-pathname
                :defaults defaults :version :newest :case :local
                :name (strcat name ".asd")
                :type "lnk")))
          (when (probe-file* shortcut)
            (let ((target (parse-windows-shortcut shortcut)))
              (when target
                (return (pathname target))))))))))

(defun* sysdef-central-registry-search (system)
  (let ((name (coerce-name system))
        (to-remove nil)
        (to-replace nil))
    (block nil
      (unwind-protect
           (dolist (dir *central-registry*)
             (let ((defaults (eval dir)))
               (when defaults
                 (cond ((directory-pathname-p defaults)
                        (let ((file (probe-asd name defaults)))
                          (when file
                            (return file))))
                       (t
                        (restart-case
                            (let* ((*print-circle* nil)
                                   (message
                                    (format nil
                                            (compatfmt "~@<While searching for system ~S: ~3i~_~S evaluated to ~S which is not a directory.~@:>")
                                            system dir defaults)))
                              (error message))
                          (remove-entry-from-registry ()
                            :report "Remove entry from *central-registry* and continue"
                            (push dir to-remove))
                          (coerce-entry-to-directory ()
                            :report (lambda (s)
                                      (format s (compatfmt "~@<Coerce entry to ~a, replace ~a and continue.~@:>")
                                              (ensure-directory-pathname defaults) dir))
                            (push (cons dir (ensure-directory-pathname defaults)) to-replace))))))))
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

(defun* make-temporary-package ()
  (flet ((try (counter)
           (ignore-errors
             (make-package (format nil "~A~D" :asdf counter)
                           :use '(:cl :asdf/interface)))))
    (do* ((counter 0 (+ counter 1))
          (package (try counter) (try counter)))
         (package package))))

(defmethod find-system ((name null) &optional (error-p t))
  (declare (ignorable name))
  (when error-p
    (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

(defmethod find-system (name &optional (error-p t))
  (find-system (coerce-name name) error-p))

(defvar *systems-being-defined* nil
  "A hash-table of systems currently being defined keyed by name, or NIL")

(defun* find-system-if-being-defined (name)
  (when *systems-being-defined*
    (gethash (coerce-name name) *systems-being-defined*)))

(defun* call-with-system-definitions (thunk)
  (if *systems-being-defined*
      (funcall thunk)
      (let ((*systems-being-defined* (make-hash-table :test 'equal)))
        (funcall thunk))))

(defmacro with-system-definitions ((&optional) &body body)
  `(call-with-system-definitions #'(lambda () ,@body)))

(defun* load-sysdef (name pathname)
  ;; Tries to load system definition with canonical NAME from PATHNAME.
  (with-system-definitions ()
    (let ((package (make-temporary-package)))
      (unwind-protect
           (handler-bind
               ((error #'(lambda (condition)
                           (error 'load-system-definition-error
                                  :name name :pathname pathname
                                  :condition condition))))
             (let ((*package* package)
                   (*default-pathname-defaults*
                    ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
                    (pathname-directory-pathname (translate-logical-pathname pathname)))
                   (external-format (encoding-external-format (detect-encoding pathname))))
               (asdf-message (compatfmt "~&~@<; ~@;Loading system definition from ~A into ~A~@:>~%")
                             pathname package)
               (load pathname #-gcl<2.7 :external-format #-gcl<2.7 external-format)))
        (delete-package package)))))

(defun* locate-system (name)
  "Given a system NAME designator, try to locate where to load the system from.
Returns five values: FOUNDP FOUND-SYSTEM PATHNAME PREVIOUS PREVIOUS-TIME
FOUNDP is true when a system was found,
either a new unregistered one or a previously registered one.
FOUND-SYSTEM when not null is a SYSTEM object that may be REGISTER-SYSTEM'ed as is
PATHNAME when not null is a path from where to load the system,
either associated with FOUND-SYSTEM, or with the PREVIOUS system.
PREVIOUS when not null is a previously loaded SYSTEM object of same name.
PREVIOUS-TIME when not null is the time at which the PREVIOUS system was loaded."
  (let* ((name (coerce-name name))
         (in-memory (system-registered-p name)) ; load from disk if absent or newer on disk
         (previous (cdr in-memory))
         (previous (and (typep previous 'system) previous))
         (previous-time (car in-memory))
         (found (search-for-system-definition name))
         (found-system (and (typep found 'system) found))
         (pathname (or (and (typep found '(or pathname string)) (pathname found))
                       (and found-system (system-source-file found-system))
                       (and previous (system-source-file previous))))
         (foundp (and (or found-system pathname previous) t)))
    (check-type found (or null pathname system))
    (when foundp
      (setf pathname (resolve-symlinks* pathname))
      (when (and pathname (not (absolute-pathname-p pathname)))
        (setf pathname (ensure-pathname-absolute pathname))
        (when found-system
          (%set-system-source-file pathname found-system)))
      (when (and previous (not (#-cormanlisp equal #+cormanlisp equalp
                                             (system-source-file previous) pathname)))
        (%set-system-source-file pathname previous)
        (setf previous-time nil))
      (values foundp found-system pathname previous previous-time))))

(defmethod find-system ((name string) &optional (error-p t))
  (with-system-definitions ()
    (loop
      (restart-case
          (multiple-value-bind (foundp found-system pathname previous previous-time)
              (locate-system name)
            (declare (ignore foundp))
            (when (and found-system (not previous))
              (register-system found-system))
            (unless (and (equal pathname (and previous (system-source-file previous)))
                         (stamp<= (safe-file-write-date pathname) previous-time))
              ;; only load when it's a different pathname, or newer file content
              (load-sysdef name pathname))
            (let ((in-memory (system-registered-p name))) ; try again after loading from disk if needed
              (return
                (cond
                  (in-memory
                   (when pathname
                     (setf (car in-memory) (safe-file-write-date pathname)))
                   (cdr in-memory))
                  (error-p
                   (error 'missing-component :requires name))))))
        (reinitialize-source-registry-and-retry ()
          :report (lambda (s)
                    (format s (compatfmt "~@<Retry finding system ~A after reinitializing the source-registry.~@:>") name))
          (initialize-source-registry))))))

(defun* find-system-fallback (requested fallback &rest keys &key source-file &allow-other-keys)
  (setf fallback (coerce-name fallback)
        requested (coerce-name requested))
  (when (equal requested fallback)
    (let ((registered (cdr (gethash fallback *defined-systems*))))
      (or registered
          (apply 'make-instance 'system
                 :name fallback :source-file source-file keys)))))

(defun* sysdef-find-asdf (name)
  ;; Bug: :version *asdf-version* won't be updated when ASDF is updated.
  (find-system-fallback name "asdf" :version (asdf-version)))
