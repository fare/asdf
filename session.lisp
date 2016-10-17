;;;; -------------------------------------------------------------------------
;;;; Session cache

(uiop/package:define-package :asdf/session
  (:recycle :asdf/session :asdf/cache)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export #:get-file-stamp #:compute-file-stamp #:register-file-stamp
           #:asdf-cache #:set-asdf-cache-entry #:unset-asdf-cache-entry #:consult-asdf-cache
           #:do-asdf-cache #:normalize-namestring
           #:call-with-asdf-session #:with-asdf-session
           #:*asdf-session* #:*asdf-session-class* #:caching-session
           #:clear-configuration-and-retry #:retry))
(in-package :asdf/session)

;;; The ASDF session cache is used to memoize some computations. It is instrumental in achieving:
;; * Consistency in the view of the world relied on by ASDF within a given session.
;;   Inconsistencies in file stamps, system definitions, etc., could cause infinite loops
;;   (a.k.a. stack overflows) and other erratic behavior.
;; * Speed and reliability of ASDF, with fewer side-effects from access to the filesystem, and
;;   no expensive recomputations of transitive dependencies for some input-files or output-files.
;; * Testability of ASDF with the ability to fake timestamps without actually touching files.

(with-upgradability ()
  ;; The session variable.
  ;; NIL when outside a session.
  (defvar *asdf-session* nil)
  (defparameter* *asdf-session-class* 'caching-session)
  (defclass caching-session ()
    ((cache :initform (make-hash-table :test 'equal) :reader session-cache)))

  (defun asdf-cache ()
    (session-cache *asdf-session*))

  ;; Set a session cache entry for KEY to a list of values VALUE-LIST, when inside a session.
  ;; Return those values.
  (defun set-asdf-cache-entry (key value-list)
    (values-list (if *asdf-session*
                     (setf (gethash key (asdf-cache)) value-list)
                     value-list)))

  ;; Unset the session cache entry for KEY, when inside a session.
  (defun unset-asdf-cache-entry (key)
    (when *asdf-session*
      (remhash key (session-cache *asdf-session*))))

  ;; Consult the session cache entry for KEY if present and in a session;
  ;; if not present, compute it by calling the THUNK,
  ;; and set the session cache entry accordingly, if in a session.
  ;; Return the values from the cache and/or the thunk computation.
  (defun consult-asdf-cache (key &optional thunk)
    (if *asdf-session*
        (multiple-value-bind (results foundp) (gethash key (session-cache *asdf-session*))
          (if foundp
              (values-list results)
              (set-asdf-cache-entry key (multiple-value-list (call-function thunk)))))
        (call-function thunk)))

  ;; Syntactic sugar for consult-asdf-cache
  (defmacro do-asdf-cache (key &body body)
    `(consult-asdf-cache ,key #'(lambda () ,@body)))

  ;; Compute inside a ASDF session with a cache.
  ;; First, make sure an ASDF session is underway, by binding the session cache variable
  ;; to a new hash-table if it's currently null (or even if it isn't, if OVERRIDE is true).
  ;; Second, if a new session was started, establish restarts for retrying the overall computation.
  ;; Finally, consult the cache if a KEY was specified with the THUNK as a fallback when the cache
  ;; entry isn't found, or just call the THUNK if no KEY was specified.
  (defun call-with-asdf-session (thunk &key override key)
    (let ((fun (if key #'(lambda () (consult-asdf-cache key thunk)) thunk)))
      (if (and *asdf-session* (not override))
          (funcall fun)
          (loop
            (restart-case
                (let ((*asdf-session* (make-instance *asdf-session-class*)))
                  (return (funcall fun)))
              (retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation.~@:>"))))
              (clear-configuration-and-retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation after resetting the configuration.~@:>")))
                (clear-configuration)))))))

  ;; Syntactic sugar for call-with-asdf-session
  (defmacro with-asdf-session ((&key key override) &body body)
    `(call-with-asdf-session #'(lambda () ,@body) :override ,override :key ,key))


  ;;; Define specific accessor for file (date) stamp.

  ;; Normalize a namestring for use as a key in the session cache.
  (defun normalize-namestring (pathname)
    (let ((resolved (resolve-symlinks*
                     (ensure-absolute-pathname
                      (physicalize-pathname pathname)
                      'get-pathname-defaults))))
      (with-pathname-defaults () (namestring resolved))))

  ;; Compute the file stamp for a normalized namestring
  (defun compute-file-stamp (normalized-namestring)
    (with-pathname-defaults ()
      (safe-file-write-date normalized-namestring)))

  ;; Override the time STAMP associated to a given FILE in the session cache.
  ;; If no STAMP is specified, recompute a new one from the filesystem.
  (defun register-file-stamp (file &optional (stamp nil stampp))
    (let* ((namestring (normalize-namestring file))
           (stamp (if stampp stamp (compute-file-stamp namestring))))
      (set-asdf-cache-entry `(get-file-stamp ,namestring) (list stamp))))

  ;; Get or compute a memoized stamp for given FILE from the session cache.
  (defun get-file-stamp (file)
    (when file
      (let ((namestring (normalize-namestring file)))
        (do-asdf-cache `(get-file-stamp ,namestring) (compute-file-stamp namestring))))))

