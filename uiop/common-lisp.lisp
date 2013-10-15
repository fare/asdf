;;;; -------------------------------------------------------------------------
;;;; Handle compatibility with multiple implementations.
;;; This file is for papering over the deficiencies and peculiarities
;;; of various Common Lisp implementations.
;;; For implementation-specific access to the system, see os.lisp instead.
;;; A few functions are defined here, but actually exported from utility;
;;; from this package only common-lisp symbols are exported.

(uiop/package:define-package :uiop/common-lisp
  (:nicknames :uoip/cl :asdf/common-lisp :asdf/cl)
  (:use #-genera :common-lisp #+genera :future-common-lisp :uiop/package)
  (:reexport :common-lisp)
  (:recycle :uiop/common-lisp :uoip/cl :asdf/common-lisp :asdf/cl :asdf)
  #+allegro (:intern #:*acl-warn-save*)
  #+cormanlisp (:shadow #:user-homedir-pathname)
  #+cormanlisp
  (:export
   #:logical-pathname #:translate-logical-pathname
   #:make-broadcast-stream #:file-namestring)
  #+gcl2.6 (:shadow #:type-of #:with-standard-io-syntax) ; causes errors when loading fasl(!)
  #+gcl2.6 (:shadowing-import-from :system #:*load-pathname*)
  #+genera (:shadowing-import-from :scl #:boolean)
  #+genera (:export #:boolean #:ensure-directories-exist)
  #+mcl (:shadow #:user-homedir-pathname))
(in-package :uiop/common-lisp)

#-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
(error "ASDF is not supported on your implementation. Please help us port it.")

;; (declaim (optimize (speed 1) (debug 3) (safety 3))) ; DON'T: trust implementation defaults.


;;;; Early meta-level tweaks

#+(or abcl (and allegro ics) (and (or clisp cmu ecl mkcl) unicode)
      clozure lispworks (and sbcl sb-unicode) scl)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :asdf-unicode *features*))

#+allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *acl-warn-save*
    (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
      excl:*warn-on-nested-reader-conditionals*))
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* nil))
  (setf *print-readably* nil))

#+clozure (in-package :ccl)
#+(and clozure windows-target) ;; See http://trac.clozure.com/ccl/ticket/1117
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'external-process-wait)
    (in-development-mode
     (defun external-process-wait (proc)
       (when (and (external-process-pid proc) (eq (external-process-%status proc) :running))
         (with-interrupts-enabled
             (wait-on-semaphore (external-process-completed proc))))
       (values (external-process-%exit-code proc)
               (external-process-%status proc))))))
#+clozure (in-package :uiop/common-lisp)


#+cormanlisp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (deftype logical-pathname () nil)
  (defun make-broadcast-stream () *error-output*)
  (defun translate-logical-pathname (x) x)
  (defun user-homedir-pathname (&optional host)
    (declare (ignore host))
    (parse-namestring (format nil "~A\\" (cl:user-homedir-pathname))))
  (defun file-namestring (p)
    (setf p (pathname p))
    (format nil "~@[~A~]~@[.~A~]" (pathname-name p) (pathname-type p))))

#+ecl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *load-verbose* nil)
  (defun use-ecl-byte-compiler-p () (and (member :ecl-bytecmp *features*) t))
  (unless (use-ecl-byte-compiler-p) (require :cmp)))

#+gcl ;; Debian's GCL 2.7 has bugs with compiling multiple-value stuff, but can run ASDF 2.011
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :ansi-cl *features*)
    (error "ASDF only supports GCL in ANSI mode. Aborting.~%"))
  (setf compiler::*compiler-default-type* (pathname "")
        compiler::*lsp-ext* ""))

#+gcl2.6
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'type-of :uiop/common-lisp)
  (shadowing-import 'system:*load-pathname* :uiop/common-lisp))

#+gcl2.6
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'type-of :uiop/common-lisp)
  (export 'system:*load-pathname* :uiop/common-lisp))

#+gcl2.6 ;; Doesn't support either logical-pathnames or output-translations.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *gcl2.6* t)
  (deftype logical-pathname () nil)
  (defun type-of (x) (class-name (class-of x)))
  (defun wild-pathname-p (path) (declare (ignore path)) nil)
  (defun translate-logical-pathname (x) x)
  (defvar *compile-file-pathname* nil)
  (defun pathname-match-p (in-pathname wild-pathname)
    (declare (ignore in-wildname wild-wildname)) nil)
  (defun translate-pathname (source from-wildname to-wildname &key)
    (declare (ignore from-wildname to-wildname)) source)
  (defun %print-unreadable-object (object stream type identity thunk)
    (format stream "#<~@[~S ~]" (when type (type-of object)))
    (funcall thunk)
    (format stream "~@[ ~X~]>" (when identity (system:address object))))
  (defmacro with-standard-io-syntax (&body body)
    `(progn ,@body))
  (defmacro with-compilation-unit (options &body body)
    (declare (ignore options)) `(progn ,@body))
  (defmacro print-unreadable-object ((object stream &key type identity) &body body)
    `(%print-unreadable-object ,object ,stream ,type ,identity (lambda () ,@body)))
  (defun ensure-directories-exist (path)
    (lisp:system (format nil "mkdir -p ~S"
                         (namestring (make-pathname :name nil :type nil :version nil :defaults path))))))

#+genera
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'ensure-directories-exist)
    (defun ensure-directories-exist (path)
      (fs:create-directories-recursively (pathname path)))))

#.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl, so we use this trick
      (read-from-string
       "(eval-when (:load-toplevel :compile-toplevel :execute)
          (ccl:define-entry-point (_getenv \"getenv\") ((name :string)) :string)
          (ccl:define-entry-point (_system \"system\") ((name :string)) :int)
          ;; Note: ASDF may expect user-homedir-pathname to provide
          ;; the pathname of the current user's home directory, whereas
          ;; MCL by default provides the directory from which MCL was started.
          ;; See http://code.google.com/p/mcl/wiki/Portability
          (defun user-homedir-pathname ()
            (ccl::findfolder #$kuserdomain #$kCurrentUserFolderType))
          (defun probe-posix (posix-namestring)
            \"If a file exists for the posix namestring, return the pathname\"
            (ccl::with-cstrs ((cpath posix-namestring))
              (ccl::rlet ((is-dir :boolean)
                          (fsref :fsref))
                (when (eq #$noerr (#_fspathmakeref cpath fsref is-dir))
                  (ccl::%path-from-fsref fsref is-dir))))))"))

#+mkcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :cmp)
  (setq clos::*redefine-class-in-place* t)) ;; Make sure we have strict ANSI class redefinition semantics


;;;; Looping
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro loop* (&rest rest)
    #-genera `(loop ,@rest)
    #+genera `(lisp:loop ,@rest))) ;; In genera, CL:LOOP can't destructure, so we use LOOP*. Sigh.


;;;; compatfmt: avoid fancy format directives when unsupported
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun frob-substrings (string substrings &optional frob)
    "for each substring in SUBSTRINGS, find occurrences of it within STRING
that don't use parts of matched occurrences of previous strings, and
FROB them, that is to say, remove them if FROB is NIL,
replace by FROB if FROB is a STRING, or if FROB is a FUNCTION,
call FROB with the match and a function that emits a string in the output.
Return a string made of the parts not omitted or emitted by FROB."
    (declare (optimize (speed 0) (safety 3) (debug 3)))
    (let ((length (length string)) (stream nil))
      (labels ((emit-string (x &optional (start 0) (end (length x)))
                 (when (< start end)
                   (unless stream (setf stream (make-string-output-stream)))
                   (write-string x stream :start start :end end)))
               (emit-substring (start end)
                 (when (and (zerop start) (= end length))
                   (return-from frob-substrings string))
                 (emit-string string start end))
               (recurse (substrings start end)
                 (cond
                   ((>= start end))
                   ((null substrings) (emit-substring start end))
                   (t (let* ((sub-spec (first substrings))
                             (sub (if (consp sub-spec) (car sub-spec) sub-spec))
                             (fun (if (consp sub-spec) (cdr sub-spec) frob))
                             (found (search sub string :start2 start :end2 end))
                             (more (rest substrings)))
                        (cond
                          (found
                           (recurse more start found)
                           (etypecase fun
                             (null)
                             (string (emit-string fun))
                             (function (funcall fun sub #'emit-string)))
                           (recurse substrings (+ found (length sub)) end))
                          (t
                           (recurse more start end))))))))
        (recurse substrings 0 length))
      (if stream (get-output-stream-string stream) "")))

  (defmacro compatfmt (format)
    #+(or gcl genera)
    (frob-substrings format `("~3i~_" #+(or genera gcl2.6) ,@'("~@<" "~@;" "~@:>" "~:>")))
    #-(or gcl genera) format))
