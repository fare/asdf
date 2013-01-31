;;;; -------------------------------------------------------------------------
;;;; Handle compatibility with multiple implementations.
;;; This file is for papering over the deficiencies and peculiarities
;;; of various Common Lisp implementations.
;;; For implementation-specific access to the system, see os.lisp instead.
;;; A few functions are defined here, but actually exported from utility;
;;; from this package only common-lisp symbols are exported.

(asdf/package:define-package :asdf/common-lisp
  (:nicknames :asdf/cl)
  (:use #-genera :common-lisp #+genera :future-common-lisp :asdf/package)
  (:reexport :common-lisp)
  (:recycle :asdf/common-lisp :asdf)
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
(in-package :asdf/common-lisp)

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

#+cormanlisp
(progn
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
  (shadow 'type-of :asdf/common-lisp)
  (shadowing-import 'system:*load-pathname* :asdf/common-lisp))

#+gcl2.6
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'type-of :asdf/common-lisp)
  (export 'system:*load-pathname* :asdf/common-lisp))

#+gcl2.6
(progn ;; Doesn't support either logical-pathnames or output-translations.
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
(unless (fboundp 'ensure-directories-exist)
  (defun ensure-directories-exist (path)
    (fs:create-directories-recursively (pathname path))))

#.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl
      (read-from-string
       "(eval-when (:compile-toplevel :load-toplevel :execute)
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
(defmacro loop* (&rest rest)
  #-genera `(loop ,@rest)
  #+genera `(lisp:loop ,@rest)) ;; In genera, CL:LOOP can't destructure, so we use LOOP*. Sigh.


;;;; compatfmt: avoid fancy format directives when unsupported
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun remove-substrings (substrings string)
    (let ((length (length string)) (stream nil))
      (labels ((emit (start end)
                 (when (and (zerop start) (= end length))
                   (return-from remove-substrings string))
                 (when (< start end)
                   (unless stream (setf stream (make-string-output-stream)))
                   (write-string string stream :start start :end end)))
               (recurse (substrings start end)
                 (cond
                   ((>= start end))
                   ((null substrings) (emit start end))
                   (t (let* ((sub (first substrings))
                             (found (search sub string :start2 start :end2 end))
                             (more (rest substrings)))
                        (cond
                          (found
                           (recurse more start found)
                           (recurse substrings (+ found (length sub)) end))
                          (t
                           (recurse more start end))))))))
        (recurse substrings 0 length))
      (if stream (get-output-stream-string stream) ""))))

(defmacro compatfmt (format)
  #+(or gcl genera)
  (remove-substrings `("~3i~_" #+(or genera gcl2.6) ,@'("~@<" "~@;" "~@:>" "~:>")) format)
  #-(or gcl genera) format)


