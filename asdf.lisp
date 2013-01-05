;; -*- mode: Common-Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; coding: utf-8 -*-
;;; This is ASDF 2.26.55: Another System Definition Facility.
;;;
;;; Feedback, bug reports, and patches are all welcome:
;;; please mail to <asdf-devel@common-lisp.net>.
;;; Note first that the canonical source for ASDF is presently
;;; <URL:http://common-lisp.net/project/asdf/>.
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git master
;;; branch is the latest development version, whereas the git release
;;; branch may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2012 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; The problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file.

#+xcvb (module ())

(cl:in-package :common-lisp-user)
#+genera (in-package :future-common-lisp-user)

#-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
(error "ASDF is not supported on your implementation. Please help us port it.")

;;;; Create and setup packages in a way that is compatible with hot-upgrade.
;;;; See https://bugs.launchpad.net/asdf/+bug/485687
;;;; See these two eval-when forms, and more near the end of the file.

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;;; Before we do anything, some implementation-dependent tweaks
  ;; (declaim (optimize (speed 1) (debug 3) (safety 3))) ; NO: trust implementation defaults.
  #+allegro
  (setf excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car)) ; need that BEFORE any mention of package ASDF as below
  #+gcl ;; Debian's GCL 2.7 has bugs with compiling multiple-value stuff, but can run ASDF 2.011
  (when (or (< system::*gcl-major-version* 2) ;; GCL 2.6 fails to fully compile ASDF at all
            (and (= system::*gcl-major-version* 2)
                 (< system::*gcl-minor-version* 7)))
    (pushnew 'ignorable pcl::*variable-declarations-without-argument*)
    (pushnew :gcl<2.7 *features*))
  #+(or abcl (and allegro ics) (and (or clisp cmu ecl mkcl) unicode)
        clozure lispworks (and sbcl sb-unicode) scl)
  (pushnew :asdf-unicode *features*)
  ;;; make package if it doesn't exist yet.
  ;;; DEFPACKAGE may cause errors on discrepancies, so we avoid it.
  (unless (find-package :asdf)
    (make-package :asdf :use '(:common-lisp))))

#+gcl (defpackage :asdf (:use :cl) ;; GCL treats defpackage magically and needs this
        #+gcl<2.7 (:shadowing-import-from :system :*load-pathname*) ;; GCL 2.6 sucks
        #+gcl<2.7 (:shadow :type-of))

(in-package :asdf)

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;;; This would belong amongst implementation-dependent tweaks above,
  ;;; except that the defun has to be in package asdf.
  #+ecl (defun use-ecl-byte-compiler-p () (and (member :ecl-bytecmp *features*) t))
  #+ecl (unless (use-ecl-byte-compiler-p) (require :cmp))
  #+mkcl (require :cmp)
  #+mkcl (setq clos::*redefine-class-in-place* t) ;; Make sure we have strict ANSI class redefinition semantics

  ;;; Package setup, step 2.
  (defvar *asdf-version* nil)
  (defvar *upgraded-p* nil)
  (defvar *asdf-verbose* nil) ; was t from 2.000 to 2.014.12.
  (defun find-symbol* (s p)
    (find-symbol (string s) p))
  ;; Strip out formatting that is not supported on Genera.
  ;; Has to be inside the eval-when to make Lispworks happy (!)
  (defun strcat (&rest strings)
    (apply 'concatenate 'string strings))
  (defmacro compatfmt (format)
    #+(or gcl genera)
    (loop :for (unsupported . replacement) :in
      (append
       '(("~3i~_" . ""))
       #+(or genera gcl<2.7) '(("~@<" . "") ("; ~@;" . "; ") ("~@:>" . "") ("~:>" . ""))) :do
      (loop :for found = (search unsupported format) :while found :do
        (setf format (strcat (subseq format 0 found) replacement
                             (subseq format (+ found (length unsupported)))))))
    format)
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. The script bin/bump-version
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of this file.
         ;; "2.345" would be an official release
         ;; "2.345.6" would be a development version in the official upstream
         ;; "2.345.0.7" would be your seventh local modification of official release 2.345
         ;; "2.345.6.7" would be your seventh local modification of development version 2.345.6
         (asdf-version "2.26.55")
         (existing-asdf (find-class 'component nil))
         (existing-version *asdf-version*)
         (already-there (equal asdf-version existing-version)))
    (unless (and existing-asdf already-there)
      (when (and existing-asdf *asdf-verbose*)
        (format *trace-output*
                (compatfmt "~&~@<; ~@;Upgrading ASDF ~@[from version ~A ~]to version ~A~@:>~%")
                existing-version asdf-version))
      (labels
          ((present-symbol-p (symbol package)
             (member (nth-value 1 (find-symbol* symbol package)) '(:internal :external)))
           (present-symbols (package)
             ;; #-genera (loop :for s :being :the :present-symbols :in package :collect s) #+genera
             (let (l)
               (do-symbols (s package)
                 (when (present-symbol-p s package) (push s l)))
               (reverse l)))
           (unlink-package (package)
             (let ((u (find-package package)))
               (when u
                 (ensure-unintern u (present-symbols u))
                 (loop :for p :in (package-used-by-list u) :do
                   (unuse-package u p))
                 (delete-package u))))
           (ensure-exists (name nicknames use)
             (let ((previous
                    (remove-duplicates
                     (mapcar #'find-package (cons name nicknames))
                     :from-end t)))
               ;; do away with packages with conflicting (nick)names
               (map () #'unlink-package (cdr previous))
               ;; reuse previous package with same name
               (let ((p (car previous)))
                 (cond
                   (p
                    (rename-package p name nicknames)
                    (ensure-use p use)
                    p)
                   (t
                    (make-package name :nicknames nicknames :use use))))))
           (intern* (symbol package)
             (intern (string symbol) package))
           (remove-symbol (symbol package)
             (let ((sym (find-symbol* symbol package)))
               (when sym
                 #-cormanlisp (unexport sym package)
                 (unintern sym package)
                 sym)))
           (ensure-unintern (package symbols)
             (loop :with packages = (list-all-packages)
               :for sym :in symbols
               :for removed = (remove-symbol sym package)
               :when removed :do
               (loop :for p :in packages :do
                 (when (eq removed (find-symbol* sym p))
                   (unintern removed p)))))
           (ensure-shadow (package symbols)
             (shadow symbols package))
           (ensure-use (package use)
             (dolist (used (package-use-list package))
               (unless (member (package-name used) use :test 'string=)
                 (unuse-package used)
                 (do-external-symbols (sym used)
                   (when (eq sym (find-symbol* sym package))
                     (remove-symbol sym package)))))
             (dolist (used (reverse use))
               (do-external-symbols (sym used)
                 (unless (eq sym (find-symbol* sym package))
                   (remove-symbol sym package)))
               (use-package used package)))
           #-ecl (ensure-fmakunbound (package symbols)
                   (loop :for name :in symbols
                         :for sym = (find-symbol* name package)
                         :when sym :do (fmakunbound sym)))
           #-ecl (ensure-fmakunbound-setf (package symbols)
                   (loop :for name :in symbols
                         :for sym = (find-symbol* name package)
                         :when sym :do #-gcl (fmakunbound `(setf ,sym))))
           (ensure-export (package export)
             (let ((formerly-exported-symbols nil)
                   (bothly-exported-symbols nil)
                   (newly-exported-symbols nil))
               (do-external-symbols (sym package)
                 (if (member sym export :test 'string-equal)
                     (push sym bothly-exported-symbols)
                     (push sym formerly-exported-symbols)))
               (loop :for sym :in export :do
                 (unless (member sym bothly-exported-symbols :test 'equal)
                   (push sym newly-exported-symbols)))
               (loop :for user :in (package-used-by-list package)
                 :for shadowing = (package-shadowing-symbols user) :do
                 (loop :for new :in newly-exported-symbols
                   :for old = (find-symbol* new user)
                   :when (and old (not (member old shadowing)))
                   :do (unintern old user)))
               (loop :for x :in newly-exported-symbols :do
                 (export (intern* x package)))))
           (ensure-package (name &key nicknames use unintern
                                 shadow export fmakunbound fmakunbound-setf)
             (let* ((p (ensure-exists name nicknames use)))
               #-ecl (ensure-fmakunbound p fmakunbound) #+ecl fmakunbound ;; do it later on ECL
               #-ecl (ensure-fmakunbound-setf p fmakunbound-setf) #+ecl fmakunbound-setf
               (ensure-unintern p unintern)
               (ensure-shadow p shadow)
               (ensure-export p export)
               p)))
        (macrolet
            ((pkgdcl (name &key nicknames use export
                           fmakunbound fmakunbound-setf unintern shadow)
                 `(ensure-package
                   ',name :nicknames ',nicknames :use ',use :export ',export
                   :shadow ',shadow :unintern ',unintern
                   :fmakunbound ',fmakunbound :fmakunbound-setf ',fmakunbound-setf)))
          (pkgdcl
           :asdf
           :use (:common-lisp)
           :fmakunbound
           (#:perform #:explain #:output-files #:operation-done-p #:traverse-action #:traverse
            #:compute-action-stamp #:component-load-dependencies #:action-valid-p
            #:component-depends-on #:perform-plan #:mark-operation-done #:operation-forced
            #:perform-with-restarts #:component-relative-pathname
            #:system-source-file #:operate #:find-component #:find-system
            #:apply-output-translations #:translate-pathname* #:resolve-location
            #:system-relative-pathname #:source-file-type #:builtin-system-p
            #:inherit-source-registry #:process-source-registry #:process-source-registry-directive
            #:compile-file* #:source-file-type #:trivial-system-p)
           :fmakunbound-setf (#:output-translations)
           :unintern
           (#:*asdf-revision* #:around #:asdf-method-combination #:split #:make-collector
            #:do-dep #:do-one-dep #:do-traverse #:visit-action #:component-visited-p
            #:loaded-systems ; makes for annoying SLIME completion
            #:output-files-for-system-and-operation) ; obsolete ASDF-BINARY-LOCATION function
           :export
           (#:defsystem #:find-system #:locate-system #:coerce-name
            #:oos #:operate #:traverse #:perform-plan
            #:system-definition-pathname #:with-system-definitions
            #:search-for-system-definition #:find-component #:component-find-path
            #:compile-system #:load-system #:load-systems
            #:require-system #:test-system #:clear-system
            #:operation #:upward-operation #:downward-operation
            #:load-op #:prepare-op #:compile-op #:load-fasl-op
            #:prepare-source-op #:load-source-op #:test-op
            #:feature #:version #:version-satisfies #:upgrade-asdf
            #:implementation-identifier #:implementation-type #:hostname
            #:input-files #:output-files #:output-file #:perform
            #:operation-done-p #:explain #:component-sibling-dependencies
            #:component-load-dependencies #:run-shell-command ; deprecated, do not use
            #:precompiled-system #:compiled-file
            #+ecl #:make-build #+mkcl #:bundle-system

            #:component #:parent-component #:child-component #:system #:module
            #:source-file #:c-source-file #:java-source-file
            #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
            #:static-file #:doc-file #:html-file :text-file
            #:source-file-type
            #:unix-dso

            #:component-children          ; component accessors
            #:component-children-by-name
            #:component-pathname
            #:component-relative-pathname
            #:component-name
            #:component-version
            #:component-parent
            #:component-property
            #:component-system
            #:component-encoding
            #:component-external-format

            #:component-depends-on ; backward-compatible name rather than action-depends-on
            #:module-components ; backward-compatibility
            #:operation-on-warnings #:operation-on-failure ; backward-compatibility

            #:system-description
            #:system-long-description
            #:system-author
            #:system-maintainer
            #:system-license
            #:system-licence
            #:system-source-file
            #:system-source-directory
            #:system-relative-pathname
            #:map-systems

            #:operation-description

            #:*system-definition-search-functions*   ; variables
            #:*central-registry*
            #:*compile-file-warnings-behaviour*
            #:*compile-file-failure-behaviour*
            #:*resolve-symlinks*
            #:*load-system-operation*
            #:*asdf-verbose*
            #:*verbose-out*

            #:asdf-version

            #:operation-error #:compile-failed #:compile-warned #:compile-error
            #:error-name
            #:error-pathname
            #:load-system-definition-error
            #:error-component #:error-operation
            #:system-definition-error
            #:missing-component
            #:missing-component-of-version
            #:missing-dependency
            #:missing-dependency-of-version
            #:circular-dependency        ; errors
            #:duplicate-names

            #:try-recompiling
            #:retry
            #:accept                     ; restarts
            #:coerce-entry-to-directory
            #:remove-entry-from-registry

            #:*encoding-detection-hook*
            #:*encoding-external-format-hook*
            #:*default-encoding*
            #:*utf-8-external-format*

            #:clear-configuration
            #:*output-translations-parameter*
            #:initialize-output-translations
            #:disable-output-translations
            #:clear-output-translations
            #:ensure-output-translations
            #:apply-output-translations
            #:compile-file*
            #:compile-file-pathname*
            #:enable-asdf-binary-locations-compatibility
            #:*default-source-registries*
            #:*source-registry-parameter*
            #:initialize-source-registry
            #:compute-source-registry
            #:clear-source-registry
            #:ensure-source-registry
            #:process-source-registry
            #:system-registered-p #:registered-systems #:already-loaded-systems
            #:resolve-location
            #:asdf-message
            #:user-output-translations-pathname
            #:system-output-translations-pathname
            #:user-output-translations-directory-pathname
            #:system-output-translations-directory-pathname
            #:user-source-registry
            #:system-source-registry
            #:user-source-registry-directory
            #:system-source-registry-directory)))
        #+genera (import 'scl:boolean :asdf)
        (setf *asdf-version* asdf-version
              *upgraded-p* (if existing-version
                               (cons existing-version *upgraded-p*)
                               *upgraded-p*))))))

;;;; -------------------------------------------------------------------------
;;;; User-visible parameters
;;;;
(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.

Defaults to T.")

(defvar *compile-file-warnings-behaviour*
  (or #+clisp :ignore :warn)
  "How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.")

(defvar *compile-file-failure-behaviour*
  (or #+(or mkcl sbcl) :error #+clisp :ignore :warn)
  "How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file?  Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.")

(defvar *verbose-out* nil)

(defparameter +asdf-methods+
  '(perform-with-restarts perform explain output-files operation-done-p))

(defvar *load-system-operation* 'load-op
  "Operation used by ASDF:LOAD-SYSTEM. By default, ASDF:LOAD-OP.
You may override it with e.g. ASDF:LOAD-FASL-OP from asdf-bundle,
or ASDF:LOAD-SOURCE-OP if your fasl loading is somehow broken.")

(defvar *compile-op-compile-file-function* 'compile-file*
  "Function used to compile lisp files.")

#+allegro
(eval-when (:compile-toplevel :execute)
  (defparameter *acl-warn-save*
                (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
                  excl:*warn-on-nested-reader-conditionals*))
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* nil)))

;;;; -------------------------------------------------------------------------
;;;; Resolve forward references

(declaim (ftype (function (t) t)
                format-arguments format-control
                error-name error-pathname error-condition
                duplicate-names-name
                error-component error-operation
                component-children component-children-by-name
                circular-dependency-actions
                condition-arguments condition-form
                condition-format condition-location
                coerce-name)
         (ftype (function (&optional t) (values)) initialize-source-registry)
         #-(or cormanlisp gcl<2.7)
         (ftype (function (t t) t) (setf component-children-by-name)))

;;;; -------------------------------------------------------------------------
;;;; Compatibility various implementations
#+cormanlisp
(progn
  (deftype logical-pathname () nil)
  (defun make-broadcast-stream () *error-output*)
  (defun translate-logical-pathname (x) x)
  (defun file-namestring (p)
    (setf p (pathname p))
    (format nil "~@[~A~]~@[.~A~]" (pathname-name p) (pathname-type p))))

#.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl
      (read-from-string
       "(eval-when (:compile-toplevel :load-toplevel :execute)
          (ccl:define-entry-point (_getenv \"getenv\") ((name :string)) :string)
          (ccl:define-entry-point (_system \"system\") ((name :string)) :int)
          ;; Note: ASDF may expect user-homedir-pathname to provide
          ;; the pathname of the current user's home directory, whereas
          ;; MCL by default provides the directory from which MCL was started.
          ;; See http://code.google.com/p/mcl/wiki/Portability
          (defun current-user-homedir-pathname ()
            (ccl::findfolder #$kuserdomain #$kCurrentUserFolderType))
          (defun probe-posix (posix-namestring)
            \"If a file exists for the posix namestring, return the pathname\"
            (ccl::with-cstrs ((cpath posix-namestring))
              (ccl::rlet ((is-dir :boolean)
                          (fsref :fsref))
                (when (eq #$noerr (#_fspathmakeref cpath fsref is-dir))
                  (ccl::%path-from-fsref fsref is-dir))))))"))

#+genera
(unless (fboundp 'ensure-directories-exist)
  (defun ensure-directories-exist (path)
    (fs:create-directories-recursively (pathname path))))

#+gcl<2.7
(progn ;; Doesn't support either logical-pathnames or output-translations.
  (deftype logical-pathname () nil)
  (defun wild-pathname-p (path) (declare (ignore path)) nil)
  (defun translate-logical-pathname (x) x)
  (defvar *compile-file-pathname* nil)
  (defun pathname-match-p (in-pathname wild-pathname)
    (declare (ignore in-wildname wild-wildname)) nil)
  (defun translate-pathname (source from-wildname to-wildname &key)
    (declare (ignore from-wildname to-wildname)) source)
  (defun type-of (x) (class-name (class-of x)))
  (defun %print-unreadable-object (object stream type identity thunk)
    (format stream "#<~@[~S ~]" (when type (type-of object)))
    (funcall thunk)
    (format stream "~@[ ~X~]>" (when identity (system:address object))))
  (defmacro with-compilation-unit (options &body body)
    (declare (ignore options)) `(progn ,@body))
  (defmacro print-unreadable-object ((object stream &key type identity) &body body)
    `(%print-unreadable-object ,object ,stream ,type ,identity (lambda () ,@body)))
  (defun ensure-directories-exist (path)
    (run-shell-command "mkdir -p ~S" (namestring (pathname-directory-pathname path)))))

;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities

(macrolet
    ((defdef (def* def)
       `(defmacro ,def* (name formals &rest rest)
          `(progn
             #+(or ecl gcl)
             ,(when (and #+gcl<2.7 (symbolp name))
                `(fmakunbound ',name))
             #-gcl ; gcl 2.7.0 notinline functions lose secondary return values :-(
             ,(when (and #+ecl (symbolp name)) ; fails for setf functions on ecl
                `(declaim (notinline ,name)))
             (,',def ,name ,formals ,@rest)))))
  (defdef defgeneric* defgeneric)
  (defdef defun* defun))

(defmacro while-collecting ((&rest collectors) &body body)
  "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

(defmacro aif (test then &optional else)
  "Anaphoric version of IF, On Lisp style"
  `(let ((it ,test)) (if it ,then ,else)))

(defun* pathname-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname :name nil :type nil :version nil :defaults pathname)))

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

(defun* make-pathname-component-logical (x)
  "Make a pathname component suitable for use in a logical-pathname"
  (typecase x
    ((eql :unspecific) nil)
    #+clisp (string (string-upcase x))
    #+clisp (cons (mapcar 'make-pathname-component-logical x))
    (t x)))

(defun* denormalize-pathname-directory-component (directory-component)
  #-gcl<2.7 directory-component
  #+gcl<2.7
  (let ((d (substitute-if :parent (lambda (x) (member x '(:up :back)))
                          directory-component)))
    (cond
      ((and (consp d) (eq :relative (first d))) (rest d))
      ((and (consp d) (eq :absolute (first d))) `(:root ,@(rest d)))
      (t d))))

(defun* make-pathname* (&key host device directory name type version defaults)
  (make-pathname
   :defaults (or defaults *default-pathname-defaults*)
   :host host :device device :name name :type type :version version
   :directory (denormalize-pathname-directory-component directory)))
   

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

(defun* pathname-parent-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname* :name nil :type nil :version nil
                    :directory (merge-pathname-directory-components
                                '(:relative :back) (pathname-directory pathname))
                    :defaults pathname)))

(define-modify-macro appendf (&rest args)
  append "Append onto list") ;; only to be used on short lists.

(define-modify-macro orf (&rest args)
  or "or a flag")

(defun* first-char (s)
  (and (stringp s) (plusp (length s)) (char s 0)))

(defun* last-char (s)
  (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

(defun* list-to-hash-set (list &aux (h (make-hash-table :test 'equal)))
  (dolist (x list h) (setf (gethash x h) t)))

(defun* asdf-message (format-string &rest format-args)
  #-gcl<2.7 (declare (dynamic-extent format-args))
  (apply 'format *verbose-out* format-string format-args))

(defun* split-string (string &key max (separator '(#\Space #\Tab)))
  "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
  (catch nil
    (let ((list nil) (words 0) (end (length string)))
      (flet ((separatorp (char) (find char separator))
             (done () (throw nil (cons (subseq string 0 end) list))))
        (loop
          :for start = (if (and max (>= words (1- max)))
                           (done)
                           (position-if #'separatorp string :end end :from-end t)) :do
          (when (null start)
            (done))
          (push (subseq string (1+ start) end) list)
          (incf words)
          (setf end start))))))

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

(defun* remove-keys (key-names args)
  (loop :for (name val) :on args :by #'cddr
    :unless (member (symbol-name name) key-names
                    :key #'symbol-name :test 'equal)
    :append (list name val)))

(defun* remove-keyword (key args)
  (loop :for (k v) :on args :by #'cddr
    :unless (eq k key)
    :append (list k v)))

(defun* getenv (x)
  (declare (ignorable x))
  #+(or abcl clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+gcl (system:getenv x)
  #+genera nil
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+mkcl (#.(or (find-symbol* 'getenv :si) (find-symbol* 'getenv :mk-ext)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S is not supported on your implementation" 'getenv))

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

(defun* absolute-pathname-p (pathspec)
  (and (typep pathspec '(or pathname string))
       (eq :absolute (car (normalize-pathname-directory-component
                           (pathname-directory (pathname pathspec)))))))

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

(defun* merge-component-name-type (name &key type defaults)
  ;; For backward-compatibility only, for people using internals.
  ;; Will be removed in a future release, e.g. 2.016.
  (warn "Please don't use ASDF::MERGE-COMPONENT-NAME-TYPE. Use ASDF:COERCE-PATHNAME.")
  (coerce-pathname name :type type :defaults defaults))

(defun* subpathname (pathname subpath &key type)
  (and pathname (merge-pathnames* (coerce-pathname subpath :type type)
                                  (pathname-directory-pathname pathname))))

(defun* subpathname* (pathname subpath &key type)
  (and pathname
       (subpathname (ensure-directory-pathname pathname) subpath :type type)))

(defun* length=n-p (x n) ;is it that (= (length x) n) ?
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

(defun* string-suffix-p (s suffix)
  (check-type s string)
  (check-type suffix string)
  (let ((start (- (length s) (length suffix))))
    (and (<= 0 start)
         (string-equal s suffix :start1 start))))

(defun* read-file-forms (file)
  (with-open-file (in file)
    (loop :with eof = (list nil)
     :for form = (read in nil eof)
     :until (eq form eof)
     :collect form)))

(defun* pathname-root (pathname)
  (make-pathname* :directory '(:absolute)
                  :name nil :type nil :version nil
                  :defaults pathname ;; host device, and on scl, *some*
                  ;; scheme-specific parts: port username password, not others:
                  . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

(defun* probe-file* (p)
  "when given a pathname P, probes the filesystem for a file or directory
with given pathname and if it exists return its truename."
  (etypecase p
    (null nil)
    (string (probe-file* (parse-namestring p)))
    (pathname (unless (wild-pathname-p p)
                #.(or #+(or allegro clozure cmu cormanlisp ecl lispworks mkcl sbcl scl)
                      '(probe-file p)
                      #+clisp (aif (find-symbol* '#:probe-pathname :ext)
                                   `(ignore-errors (,it p)))
                      #+gcl<2.7
                      '(or (probe-file p)
                        (and (directory-pathname-p p)
                         (ignore-errors
                          (ensure-directory-pathname
                           (truename (subpathname (ensure-directory-pathname p) "."))))))
                      '(ignore-errors (truename p)))))))

(defun* truenamize (pathname &optional (defaults *default-pathname-defaults*))
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep pathname '(or null logical-pathname)) (return pathname))
    (let ((p (merge-pathnames* pathname defaults)))
      (when (typep p 'logical-pathname) (return p))
      (let ((found (probe-file* p)))
        (when found (return found)))
      (unless (absolute-pathname-p p)
        (let ((true-defaults (ignore-errors (truename defaults))))
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

(defun* ensure-pathname-absolute (path)
  (cond
    ((absolute-pathname-p path) path)
    ((stringp path) (ensure-pathname-absolute (pathname path)))
    ((not (pathnamep path)) (error "not a valid pathname designator ~S" path))
    (t (let ((resolved (resolve-symlinks path)))
         (assert (absolute-pathname-p resolved))
         resolved))))

(defun* default-directory ()
  (truenamize (pathname-directory-pathname *default-pathname-defaults*)))

(defun* lispize-pathname (input-file)
  (make-pathname :type "lisp" :defaults input-file))

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

(defun* add-pathname-suffix (pathname suffix)
  (make-pathname :name (strcat (pathname-name pathname) suffix)
                 :defaults pathname))

(defun* tmpize-pathname (x)
  (add-pathname-suffix x "-ASDF-TMP"))

(defun* rename-file-overwriting-target (source target)
  #+clisp ;; But for a bug in CLISP 2.48, we should use :if-exists :overwrite and be atomic
  (posix:copy-file source target :method :rename)
  #-clisp
  (rename-file source target
               #+clozure :if-exists #+clozure :rename-and-delete))

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

(defun* stamp< (x y)
  (etypecase x
    (null (and y t))
    ((eql t) nil)
    (real (etypecase y
            (null nil)
            ((eql t) t)
            (real (< x y))))))
;;(defun* stamps< (list) (loop :for y :in list :for x = nil :then y :always (stamp< x y)))
;;(defun* stamp*< (&rest list) (stamps< list))
(defun* stamp<= (x y) (not (stamp< y x)))
(defun* earlier-stamp (x y) (if (stamp< x y) x y))
(defun* stamps-earliest (list) (reduce 'earlier-stamp list :initial-value t))
(defun* earliest-stamp (&rest list) (stamps-earliest list))
(defun* later-stamp (x y) (if (stamp< x y) y x))
(defun* stamps-latest (list) (reduce 'later-stamp list :initial-value nil))
(defun* latest-stamp (&rest list) (stamps-latest list))
(define-modify-macro latest-stamp-f (&rest stamps) latest-stamp)

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

;;;; -------------------------------------------------------------------------
;;;; ASDF Interface, in terms of generic functions.
(defgeneric* find-system (system &optional error-p))
(defgeneric* perform-with-restarts (operation component))
(defgeneric* perform (operation component))
(defgeneric* operation-done-p (operation component) ;; ASDF3: eliminate?
  (:documentation "Returns a boolean, which is NIL if the action is forced to be performed again"))
(defgeneric* mark-operation-done (operation component)) ;; ASDF3: rename to stamp-action
(defgeneric* explain (operation component))
(defgeneric* output-files (operation component))
(defgeneric* input-files (operation component))
(defgeneric* component-operation-time (operation component))
(defgeneric* component-description (component))
(defgeneric* operation-description (operation component) ;; ASDF3: rename to action-description
  (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))

(defgeneric* system-source-file (system)
  (:documentation "Return the source file in which system is defined."))

(defgeneric* component-name (component)
  (:documentation "Name of the COMPONENT, unique relative to its parent"))

(defgeneric* component-system (component)
  (:documentation "Find the top-level system containing COMPONENT"))

(defgeneric* component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defgeneric* component-relative-pathname (component)
  (:documentation "Returns a pathname for the component argument intended to be
interpreted relative to the pathname of that component's parent.
Despite the function's name, the return value may be an absolute
pathname, because an absolute pathname may be interpreted relative to
another pathname in a degenerate way."))

(defgeneric* component-property (component property))

#-gcl<2.7
(defgeneric* (setf component-property) (new-value component property))

(defgeneric* component-external-format (component))

(defgeneric* component-encoding (component))

#-gcl<2.7
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric* (setf component-children-by-name) (new-value parent)))

(defgeneric* version-satisfies (component version))

(defgeneric* find-component (base path)
  (:documentation "Find a component by resolving the PATH starting from BASE parent"))

(defgeneric* find-operation (base path)
  (:documentation "Find an operation by resolving the PATH starting from BASE parent"))

(defgeneric* source-file-type (component system))

(defgeneric* operation-ancestor (operation)
  (:documentation
   "Recursively chase the operation's parent pointer until we get to
the head of the tree"))

(defgeneric* component-depends-on (operation component) ;; ASDF3: rename to component-dependencies
  (:documentation
   "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is a class
        designator and each <component> is a component
        designator, which means that the component depends on
        <operation> having been performed on each <component>; or

      (FEATURE <feature>), which means that the component depends
        on <feature>'s presence in *FEATURES*.

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the
    list."))

(defgeneric* component-self-dependencies (operation component))

(defgeneric* traverse (operation component &key force force-not verbose)
  (:documentation
"Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))

(defgeneric* compute-action-stamp (plan operation component &key just-done)
  (:documentation "Has this action been successfully done already,
and at what known timestamp has it been done at or will it be done at?
Takes two keywords JUST-DONE and PLAN:
JUST-DONE is a boolean that is true if the action was just successfully performed,
at which point we want compute the actual stamp and warn if files are missing;
otherwise we are making plans, anticipating the effects of the action.
PLAN is a plan object modelling future effects of actions,
or NIL to denote what actually happened.
Returns two values:
* a STAMP saying when it was done or will be done,
  or T if the action has involves files that need to be recomputed.
* a boolean DONE-P that indicates whether the action has actually been done,
  and both its output-files and its in-image side-effects are up to date."))

(defgeneric* builtin-system-p (system))
(defgeneric* action-forced-p (plan operation component))
(defgeneric* action-forced-not-p (plan operation component))

;; for backward-compatibility only. Used by swank.asd for swank-loader.
(defgeneric* operation-forced (operation))

;;;; -------------------------------------------------------------------------
;;; Methods in case of hot-upgrade. See https://bugs.launchpad.net/asdf/+bug/485687
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun* compute-children-by-name (module)
    (let ((hash (make-hash-table :test 'equal)))
      (setf (component-children-by-name module) hash)
      (loop :for c :in (component-children module)
            :for name = (component-name c)
            :for previous = (gethash name hash)
            :do (when previous (error 'duplicate-names :name name))
                (setf (gethash name hash) c))
      hash))
  (when *upgraded-p*
    ;; override previous definition from 2.018 to 2.26, not needed
    ;; since we've stopped trying to recycle previously-installed systems.
    (handler-bind ((style-warning #'muffle-warning))
      (when (find-class 'operation nil)
        (eval '(defmethod shared-initialize :after ((o operation) slot-names &rest initargs &key)
                (declare (ignorable o slot-names initargs)) (values))))
      (when (find-class 'component nil)
        (eval '(defmethod reinitialize-instance :after ((c component) &rest initargs &key)
                (declare (ignorable c initargs)) (values))))
      (when (find-class 'module nil)
        (eval '(defmethod reinitialize-instance :after ((m module) &rest initargs &key)
                (declare (ignorable m initargs)) (values)))
        (eval '(defmethod update-instance-for-redefined-class :after
                ((m module) added deleted plist &key)
                (declare (ignorable m added deleted plist))
                (when (and (member 'children added) (member 'components deleted))
                  (setf (slot-value m 'children)
                        ;; old ECLs provide an alist instead of a plist(!)
                        (if (or #+ecl (consp (first plist))) (or #+ecl (cdr (assoc 'components plist)))
                            (getf plist 'components)))
                  (compute-children-by-name m))
                (when (typep m 'system)
                  (when (member 'source-file added)
                    (%set-system-source-file
                     (probe-asd (component-name m) (component-pathname m)) m))
                  (when (equal (component-name m) "asdf")
                    (setf (component-version m) *asdf-version*)))))))))

;;;; -------------------------------------------------------------------------
;;; Classes, Conditions

(define-condition system-definition-error (error) ()
  ;; [this use of :report should be redundant, but unfortunately it's not.
  ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
  ;; over print-object; this is always conditions::%print-condition for
  ;; condition objects, which in turn does inheritance of :report options at
  ;; run-time.  fortunately, inheritance means we only need this kludge here in
  ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
  #+cmu (:report print-object))

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

(define-condition circular-dependency (system-definition-error)
  ((actions :initarg :actions :reader circular-dependency-actions))
  (:report (lambda (c s)
             (format s (compatfmt "~@<Circular dependency: ~3i~_~S~@:>")
                     (circular-dependency-actions c)))))

(define-condition duplicate-names (system-definition-error)
  ((name :initarg :name :reader duplicate-names-name))
  (:report (lambda (c s)
             (format s (compatfmt "~@<Error while defining system: multiple components are given same name ~A~@:>")
                     (duplicate-names-name c)))))

(define-condition missing-component (system-definition-error)
  ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
   (parent :initform nil :reader missing-parent :initarg :parent)))

(define-condition missing-component-of-version (missing-component)
  ((version :initform nil :reader missing-version :initarg :version)))

(define-condition missing-dependency (missing-component)
  ((required-by :initarg :required-by :reader missing-required-by)))

(define-condition missing-dependency-of-version (missing-dependency
                                                 missing-component-of-version)
  ())

(define-condition operation-error (error) ;; OBSOLETE NAME.
  ;; We want to rename it to operation-error, but that breaks upgrade on SBCL.
  ;; Before to rename it, fix these culprits:
  ;; cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
               (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                       (type-of c) (error-operation c) (error-component c)))))

(define-condition compile-error (operation-error) ())
(define-condition compile-failed (compile-error) ())
(define-condition compile-warned (compile-error) ())

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
(define-condition invalid-source-registry (invalid-configuration warning)
  ((format :initform (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))
(define-condition invalid-output-translation (invalid-configuration warning)
  ((format :initform (compatfmt "~@<Invalid asdf output-translation ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

(defclass component ()
  ((name :accessor component-name :initarg :name :type string :documentation
         "Component name: designator for a string composed of portable pathname characters")
   ;; We might want to constrain version with
   ;; :type (and string (satisfies parse-version))
   ;; but we cannot until we fix all systems that don't use it correctly!
   (version :accessor component-version :initarg :version)
   (description :accessor component-description :initarg :description)
   (long-description :accessor component-long-description :initarg :long-description)
   (sibling-dependencies :accessor component-sibling-dependencies :initform nil)
   (if-feature :accessor component-if-feature :initform nil :initarg :if-feature)
   ;; In the ASDF object model, dependencies exist between *actions*
   ;; (an action is a pair of operation and component).
   ;; Dependencies are represented as alists of operations
   ;; to dependencies (other actions) in each component.
   ;; Up until ASDF 2.26.9, there used to be two kinds of dependencies:
   ;; in-order-to and do-first, each stored in its own slot. Now there is only in-order-to.
   ;; in-order-to used to represent things that modify the filesystem (such as compiling a fasl)
   ;; and do-first things that modify the current image (such as loading a fasl).
   ;; These are now unified because we now correctly propagate timestamps between dependencies.
   ;; Happily, no one seems to have used do-first too much, but the name in-order-to remains.
   ;; The names are bad, but they have been the official API since Dan Barlow's ASDF 1.52!
   ;; LispWorks's defsystem has caused-by and requires for in-order-to and do-first respectively.
   ;; Maybe rename the slots in ASDF? But that's not very backward-compatible.
   ;; See our ASDF 2 paper for more complete explanations.
   (in-order-to :initform nil :initarg :in-order-to
                :accessor component-in-order-to)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)
   ;; the absolute-pathname is computed based on relative-pathname...
   (absolute-pathname)
   (operation-times :initform (make-hash-table)
                    :accessor component-operation-times)
   (around-compile :initarg :around-compile)
   (%encoding :accessor %component-encoding :initform nil :initarg :encoding)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties :accessor component-properties :initarg :properties
               :initform nil)
   ;; For backward-compatibility, this slot is part of component rather than child-component
   (parent :initarg :parent :initform nil :reader component-parent)))


;; Components are organized in a hierarchical tree,
;; that typically (but not necessarily) follows the filesystem hierarchy.
(defclass child-component (component) ())

(defclass parent-component (component)
  ((children
    :initform nil
    :initarg :components
    :reader module-components ; backward-compatibility
    :accessor component-children)
   (children-by-name
    :reader module-components-by-name ; backward-compatibility
    :accessor component-children-by-name)
   (default-component-class
    :initform nil
    :initarg :default-component-class
    :accessor module-default-component-class)))

(defclass module (child-component parent-component)
  ())

(defun* component-load-dependencies (component)
  ;; Old deprecated name for the same thing. Please update your software.
  (component-sibling-dependencies component))

(defun* component-find-path (component)
  (check-type component (or null component))
  (reverse
   (loop :for c = component :then (component-parent c)
         :while c :collect (component-name c))))

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity nil)
    (format stream "~{~S~^ ~}" (component-find-path c))))


;;;; methods: conditions

(defmethod print-object ((c missing-dependency) s)
  (format s (compatfmt "~@<~A, required by ~A~@:>")
          (call-next-method c nil) (missing-required-by c)))

(defun* sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error :format-control
         format :format-arguments arguments))

;;;; methods: components

(defmethod print-object ((c missing-component) s)
  (format s (compatfmt "~@<Component ~S not found~@[ in ~A~]~@:>")
          (missing-requires c)
          (when (missing-parent c)
            (coerce-name (missing-parent c)))))

(defmethod print-object ((c missing-component-of-version) s)
  (format s (compatfmt "~@<Component ~S does not match version ~A~@[ in ~A~]~@:>")
          (missing-requires c)
          (missing-version c)
          (when (missing-parent c)
            (coerce-name (missing-parent c)))))

(defmethod component-system ((component component))
  (aif (component-parent component)
       (component-system it)
       component))

(defvar *default-component-class* 'cl-source-file)

(defun* component-parent-pathname (component)
  ;; No default anymore (in particular, no *default-pathname-defaults*).
  ;; If you force component to have a NULL pathname, you better arrange
  ;; for any of its children to explicitly provide a proper absolute pathname
  ;; wherever a pathname is actually wanted.
  (let ((parent (component-parent component)))
    (when parent
      (component-pathname parent))))

(defmethod component-pathname ((component component))
  (if (slot-boundp component 'absolute-pathname)
      (slot-value component 'absolute-pathname)
      (let ((pathname
             (merge-pathnames*
              (component-relative-pathname component)
              (pathname-directory-pathname (component-parent-pathname component)))))
        (unless (or (null pathname) (absolute-pathname-p pathname))
          (error (compatfmt "~@<Invalid relative pathname ~S for component ~S~@:>")
                 pathname (component-find-path component)))
        (setf (slot-value component 'absolute-pathname) pathname)
        pathname)))

(defmethod component-property ((c component) property)
  (cdr (assoc property (slot-value c 'properties) :test #'equal)))

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
    (if a
        (setf (cdr a) new-value)
        (setf (slot-value c 'properties)
              (acons property new-value (slot-value c 'properties)))))
  new-value)

(defvar *default-encoding* :default
  "Default encoding for source files.
The default value :default preserves the legacy behavior.
A future default might be :utf-8 or :autodetect
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.")

(defparameter *utf-8-external-format*
  #+(and asdf-unicode (not clisp)) :utf-8
  #+(and asdf-unicode clisp) charset:utf-8
  #-asdf-unicode :default
  "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

(defun* always-default-encoding (pathname)
  (declare (ignore pathname))
  *default-encoding*)

(defvar *encoding-detection-hook* #'always-default-encoding
  "Hook for an extension to define a function to automatically detect a file's encoding")

(defun* detect-encoding (pathname)
  (funcall *encoding-detection-hook* pathname))

(defmethod component-encoding ((c component))
  (or (loop :for x = c :then (component-parent x)
        :while x :thereis (%component-encoding x))
      (detect-encoding (component-pathname c))))

(defun* default-encoding-external-format (encoding)
  (case encoding
    (:default :default) ;; for backward-compatibility only. Explicit usage discouraged.
    (:utf-8 *utf-8-external-format*)
    (otherwise
     (cerror "Continue using :external-format :default" (compatfmt "~@<Your ASDF component is using encoding ~S but it isn't recognized. Your system should :defsystem-depends-on (:asdf-encodings).~:>") encoding)
     :default)))

(defvar *encoding-external-format-hook*
  #'default-encoding-external-format
  "Hook for an extension to define a mapping between non-default encodings
and implementation-defined external-format's")

(defun* encoding-external-format (encoding)
  (funcall *encoding-external-format-hook* encoding))

(defmethod component-external-format ((c component))
  (encoding-external-format (component-encoding c)))

(defclass proto-system () ; slots to keep when resetting a system
  ;; To preserve identity for all objects, we'd need keep the components slots
  ;; but also to modify parse-component-form to reset the recycled objects.
  ((name) (source-file) #|(children) (children-by-names)|#))

;; Backward-compatibility: inherit from module. ASDF3: only inherit from parent-component.
(defclass system (module proto-system)
  (;; {,long-}description is now inherited from component, but we add the legacy accessors
   (description :accessor system-description)
   (long-description :accessor system-long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence
            :accessor system-license :initarg :license)
   (source-file :initarg :source-file :writer %set-system-source-file) ; upgrade issues on CLISP, CMUCL
   (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on)))

(defmethod component-pathname ((system system))
  (if (or (slot-boundp system 'relative-pathname)
            (slot-boundp system 'absolute-pathname)
            (slot-value system 'source-file))
    (call-next-method)
    (default-directory)))

;;;; -------------------------------------------------------------------------
;;;; version-satisfies

(defmethod version-satisfies ((c component) version)
  (unless (and version (slot-boundp c 'version))
    (when version
      (warn "Requested version ~S but component ~S has no version" version c))
    (return-from version-satisfies t))
  (version-satisfies (component-version c) version))

(defun* asdf-version ()
  "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.:
(ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"2.345.67\")."
  *asdf-version*)

(defun* parse-version (string &optional on-error)
  "Parse a version string as a series of natural integers separated by dots.
Return a (non-null) list of integers if the string is valid, NIL otherwise.
If on-error is error, warn, or designates a function of compatible signature,
the function is called with an explanation of what is wrong with the argument.
NB: ignores leading zeroes, and so doesn't distinguish between 2.003 and 2.3"
  (and
   (or (stringp string)
       (when on-error
         (funcall on-error "~S: ~S is not a string"
                  'parse-version string)) nil)
   (or (loop :for prev = nil :then c :for c :across string
         :always (or (digit-char-p c)
                     (and (eql c #\.) prev (not (eql prev #\.))))
         :finally (return (and c (digit-char-p c))))
       (when on-error
         (funcall on-error "~S: ~S doesn't follow asdf version numbering convention"
                  'parse-version string)) nil)
   (mapcar #'parse-integer (split-string string :separator "."))))

(defmethod version-satisfies ((cver string) version)
  (let ((x (parse-version cver 'warn))
        (y (parse-version version 'warn)))
    (labels ((bigger (x y)
               (cond ((not y) t)
                     ((not x) nil)
                     ((> (car x) (car y)) t)
                     ((= (car x) (car y))
                      (bigger (cdr x) (cdr y))))))
      (and x y (= (car x) (car y))
           (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

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

;;;; -------------------------------------------------------------------------
;;;; Finding systems

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

(defun* featurep (x &optional (features *features*))
  (cond
    ((atom x)
     (and (member x features) t))
    ((eq :not (car x))
     (assert (null (cddr x)))
     (not (featurep (cadr x) features)))
    ((eq :or (car x))
     (some #'(lambda (x) (featurep x features)) (cdr x)))
    ((eq :and (car x))
     (every #'(lambda (x) (featurep x features)) (cdr x)))
    (t
     (error "Malformed feature specification ~S" x))))

(defun* os-unix-p ()
  (featurep '(:or :unix :cygwin :darwin)))

(defun* os-windows-p ()
  (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32))))

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
                           :use '(:cl :asdf)))))
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
(defvar *systems-being-operated* nil
  "A boolean indicating that some systems are being operated on")

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
  (find-system-fallback name "asdf" :version *asdf-version*))


;;;; -------------------------------------------------------------------------
;;;; Finding components

(defmethod find-component ((base string) path)
  (let ((s (find-system base nil)))
    (and s (find-component s path))))

(defmethod find-component ((base symbol) path)
  (cond
    (base (find-component (coerce-name base) path))
    (path (find-component path nil))
    (t    nil)))

(defmethod find-component ((base cons) path)
  (find-component (car base) (cons (cdr base) path)))

(defmethod find-component ((parent parent-component) (name string))
  (unless (slot-boundp parent 'children-by-name) ;; SBCL may miss the u-i-f-r-c method!!!
    (compute-children-by-name parent))
  (values (gethash name (component-children-by-name parent))))

(defmethod find-component (base (name symbol))
  (if name
      (find-component base (coerce-name name))
      base))

(defmethod find-component ((c component) (name cons))
  (find-component (find-component c (car name)) (cdr name)))

(defmethod find-component (base (actual component))
  (declare (ignorable base))
  actual)

(defgeneric* resolve-dependency-combination (component combinator arguments))

(defun* resolve-dependency-name (component name &optional version)
  (loop
    (restart-case
        (return
          (let ((comp (find-component (component-parent component) name)))
            (unless comp
              (error 'missing-dependency
                     :required-by component
                     :requires name))
            (when version
              (unless (version-satisfies comp version)
                (error 'missing-dependency-of-version
                       :required-by component
                       :version version
                       :requires name)))
            comp))
      (retry ()
        :report (lambda (s)
                  (format s (compatfmt "~@<Retry loading ~3i~_~A.~@:>") name))
        :test
        (lambda (c)
          (or (null c)
              (and (typep c 'missing-dependency)
                   (eq (missing-required-by c) component)
                   (equal (missing-requires c) name))))))))

(defun* resolve-dependency-spec (component dep-spec)
  (if (atom dep-spec)
      (resolve-dependency-name component dep-spec)
      (resolve-dependency-combination component (car dep-spec) (cdr dep-spec))))

(defmethod resolve-dependency-combination (component combinator arguments)
  (error (compatfmt "~@<Bad dependency ~S for ~S~@:>")
         (cons combinator arguments) component))

(defmethod resolve-dependency-combination (component (combinator (eql :feature)) arguments)
  (declare (ignorable combinator))
  (when (featurep (first arguments))
    (resolve-dependency-spec component (second arguments))))

(defmethod resolve-dependency-combination (component (combinator (eql :version)) arguments)
  (declare (ignorable combinator)) ;; See https://bugs.launchpad.net/asdf/+bug/527788
  (resolve-dependency-name component (first arguments) (second arguments)))

;;; component subclasses

(defclass file-component (child-component)
  ((type :accessor file-type :initarg :type))) ; no default
(defclass source-file (file-component)
  ((type :initform nil))) ;; NB: many systems have come to rely on this default.
(defclass cl-source-file (source-file)
  ((type :initform "lisp")))
(defclass cl-source-file.cl (cl-source-file)
  ((type :initform "cl")))
(defclass cl-source-file.lsp (cl-source-file)
  ((type :initform "lsp")))
(defclass c-source-file (source-file)
  ((type :initform "c")))
(defclass java-source-file (source-file)
  ((type :initform "java")))
(defclass static-file (source-file)
  ((type :initform nil)))
(defclass doc-file (static-file) ())
(defclass html-file (doc-file)
  ((type :initform "html")))

(defmethod source-file-type ((component parent-component) system) ; not just for source-file. ASDF3: rename.
  (declare (ignorable component system))
  :directory)
(defmethod source-file-type ((component file-component) system)
  (declare (ignorable system))
  (file-type component))

(defmethod component-relative-pathname ((component component))
  (coerce-pathname
   (or (slot-value component 'relative-pathname)
       (component-name component))
   :type (source-file-type component (component-system component))
   :defaults (component-parent-pathname component)))

;;;; -------------------------------------------------------------------------
;;;; Operations

;;; one of these is instantiated whenever #'operate is called

(defclass operation ()
  ((original-initargs ;; for backward-compat -- used by GBBopen and swank (via operation-forced)
    :initform nil :initarg :original-initargs :accessor operation-original-initargs)))

(defmethod initialize-instance :after ((o operation) &rest initargs
                                       &key force force-not system verbose &allow-other-keys)
  (declare (ignorable force force-not system verbose))
  (unless (slot-boundp o 'original-initargs)
    (setf (operation-original-initargs o) initargs)))

(defmethod print-object ((o operation) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (ignore-errors
      (format stream "~{~S~^ ~}" (operation-original-initargs o)))))

(defparameter *operations* (make-hash-table :test 'equal))
(defun* make-operation (operation-class &rest initargs)
  (let ((key (cons operation-class initargs)))
    (multiple-value-bind (operation foundp) (gethash key *operations*)
    (if foundp operation
        (setf (gethash key *operations*)
              (apply 'make-instance operation-class initargs))))))
(defmethod find-operation (context (sub operation))
  (declare (ignorable context))
  sub)
(defmethod find-operation (context (sub symbol))
  (apply 'make-operation sub (operation-original-initargs context)))
(defmethod operation-original-initargs ((op null)) op)

(defmethod operation-forced ((o operation)) (getf (operation-original-initargs o) :force))

(defmethod component-depends-on ((o operation) (c component))
  (cdr (assoc (type-of o) (component-in-order-to c)))) ; User-specified in-order dependencies

(defmethod component-self-dependencies ((o operation) (c component))
  (loop :for (o-spec . c-spec) :in (component-depends-on o c)
        :unless (eq o-spec 'feature) ;; avoid the FEATURE "feature"
          :when (find c c-spec :key #'(lambda (dep) (resolve-dependency-spec c dep)))
            :collect (cons (find-operation o o-spec) c)))

(defmethod output-files ((o operation) (c component))
  (declare (ignorable o c))
  nil)

(defun* output-file (operation component)
  "The unique output file of performing OPERATION on COMPONENT"
  (let ((files (output-files operation component)))
    (assert (length=n-p files 1))
    (first files)))

(defmethod input-files ((o operation) (c parent-component))
  (declare (ignorable o c))
  nil)

(defmethod input-files ((o operation) (c file-component))
  (or (loop :for (dep-o) :in (component-self-dependencies o c)
            :append (or (output-files dep-o c) (input-files dep-o c)))
      ;; no non-trivial previous operations needed?
      ;; I guess we work with the original source file, then
      (list (component-pathname c))))

(defmethod perform :after ((o operation) (c component))
  (mark-operation-done o c))

(defmethod operation-done-p ((o operation) (c component))
  (declare (ignorable o c))
  t)

(deftype action () '(cons operation component)) ;; a step to be performed while building the system

;;; upward-operation, downward-operation and sibling-operation work together
;;; to handle modules and files along the module hierarchy:

;; Downward operations like load-op or compile-op propagate down the component hierarchy:
;; operation on a parent depends-on operation on its children.
(defclass downward-operation (operation) ())
(defmethod component-depends-on ((o downward-operation) (c parent-component))
  `((,o ,@(component-children c)) ,@(call-next-method)))

;; Upward operations, like prepare-op, propagate up the component hierarchy:
;; operation on a child depends-on operation on its parent.
;; For backward-compatibility reasons, a system inherits from module and is a child-component
;; so we must guard against this case. ASDF3: remove that.
(defclass upward-operation (operation) ())
(defmethod component-depends-on ((o upward-operation) (c child-component))
  `(,@(aif (component-parent c) `((,o ,it))) ,@(call-next-method)))

;; Our default operations: loading into the current lisp image
(defclass basic-load-op (operation) ())
(defclass load-op (basic-load-op downward-operation) ())
(defclass prepare-op (upward-operation) ())

(defmethod component-depends-on ((o prepare-op) (c component))
  (declare (ignorable o))
  `((load-op ,@(loop :for dep :in (component-sibling-dependencies c)
                     :collect (resolve-dependency-spec c dep)))
    ,@(call-next-method)))

(defmethod perform ((o prepare-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-op) (s system))
  (declare (ignorable o))
  (aif (system-source-file s) (list it)))


;;; The TRAVERSE protocol is as follows: we pass around operation, dependency,
;;; and bunch of other stuff. Functions return a stamp.
;;; The returned stamp is T if anything has changed that required a rebuild.

(defun* action-override-p (plan operation component override-accessor)
  (declare (ignorable operation))
  (let* ((override (funcall override-accessor plan)))
    (and override
         (if (typep override 'hash-table)
             (gethash (coerce-name (component-system (find-component () component))) override)
             t))))

(defgeneric* needed-in-image-p (operation component))
(defmethod needed-in-image-p ((o operation) (c component))
  ;; We presume that actions that modify the filesystem don't need be run
  ;; in the current image if they have already been done in another,
  ;; and can be run in another process (e.g. a fork),
  ;; whereas those that don't are meant to side-effect the current image and can't.
  (not (output-files o c)))

(defclass plan-traversal ()
  ((forced :initform nil :initarg :force :accessor plan-forced)
   (forced-not :initform nil :initarg :force-not :accessor plan-forced-not)
   (total-action-count :initform 0 :accessor plan-total-action-count)
   (planned-action-count :initform 0 :accessor plan-planned-action-count)
   (planned-output-action-count :initform 0 :accessor plan-planned-output-action-count)
   (visited-actions :initform (make-hash-table :test 'equal) :accessor plan-visited-actions)
   (visiting-action-set :initform (make-hash-table :test 'equal) :accessor plan-visiting-action-set)
   (visiting-action-list :initform () :accessor plan-visiting-action-list)))

(defun* normalize-forced-systems (x system)
  (etypecase x
    ((member nil :all) x)
    (cons (list-to-hash-set (mapcar #'coerce-name x)))
    ((eql t) (list-to-hash-set (list (coerce-name system))))))

(defmethod initialize-instance :after ((plan plan-traversal)
                                       &key (force () fp) (force-not () fnp) system &allow-other-keys)
  (with-slots (forced forced-not) plan
    (when fp (setf forced (normalize-forced-systems force system)))
    (when fnp (setf forced-not (normalize-forced-systems force-not system)))))

(defclass sequential-plan (plan-traversal)
  ((actions-r :initform nil :accessor plan-actions-r)))

(defgeneric* plan-action-status (plan operation component)
  (:documentation "Returns the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

(defgeneric* (setf plan-action-status) (new-status plan operation component)
  (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

(defgeneric* plan-record-dependency (plan operation component))

(defgeneric* visiting-action-p (plan operation component))

(defgeneric* (setf visiting-action-p) (new-value plan operation component))

(defclass action-status ()
  ((stamp
    :initarg :stamp :reader action-stamp
    :documentation "STAMP associated with the ACTION if it has been completed already
in some previous image, or T if it needs to be done.")
   (done-p
    :initarg :done-p :reader action-done-p
    :documentation "a boolean, true iff the action was already done (before any planned action)."))
  (:documentation "Status of an action"))

(defmethod print-object ((status action-status) stream)
  (print-unreadable-object (status stream :type t)
    (with-slots (stamp done-p) status
      (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p))))

(defclass planned-action-status (action-status)
  ((planned-p
    :initarg :planned-p :reader action-planned-p
    :documentation "a boolean, true iff the action was included in the plan.")
   (index
    :initarg :index :reader action-index
    :documentation "an integer, counting all traversed actions in traversal order."))
  (:documentation "Status of an action in a plan"))

(defmethod print-object ((status planned-action-status) stream)
  (print-unreadable-object (status stream :type t)
    (with-slots (stamp done-p planned-p) status
      (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p :planned-p planned-p))))

;; TODO: eliminate NODE-FOR, use CONS.
;; Supposes cleaner protocol for operation initargs passed to MAKE-OPERATION.
(defun* node-for (o c) (cons (type-of o) c))

(defmethod (setf plan-action-status) (new-status (plan plan-traversal) (o operation) (c component))
  (setf (gethash (node-for o c) (plan-visited-actions plan)) new-status))

(defmethod plan-action-status ((plan plan-traversal) (o operation) (c component))
  (or (and (action-forced-not-p plan o c) (plan-action-status nil o c))
      (values (gethash (node-for o c) (plan-visited-actions plan)))))

(defmethod component-operation-time ((o operation) (c component))
  (gethash (type-of o) (component-operation-times c)))

(defmethod mark-operation-done ((o operation) (c component))
  (setf (gethash (type-of o) (component-operation-times c))
        (compute-action-stamp nil o c :just-done t)))

(defun* action-already-done-p (plan operation component)
  (action-done-p (plan-action-status plan operation component)))

(defmethod plan-action-status ((plan null) (o operation) (c component))
  (declare (ignorable plan))
  (multiple-value-bind (stamp done-p) (component-operation-time o c)
    (make-instance 'action-status :stamp stamp :done-p done-p)))

(defmethod (setf plan-action-status) (new-status (plan null) (o operation) (c component))
  (declare (ignorable plan))
  (let ((to (type-of o))
        (times (component-operation-times c)))
    (if (action-done-p new-status)
        (remhash to times)
        (setf (gethash to times) (action-stamp new-status))))
  new-status)

(defmethod action-forced-p (plan operation component)
  (and (action-override-p plan operation component 'plan-forced)
       (not (builtin-system-p (component-system component)))))
(defmethod action-forced-not-p (plan operation component)
  (and (action-override-p plan operation component 'plan-forced-not)
       (not (action-forced-p plan operation component))))

(defgeneric action-valid-p (plan operation component)
  (:documentation "Is this action valid to include amongst dependencies?"))
(defmethod action-valid-p (plan operation (c component))
  (declare (ignorable plan operation))
  (aif (component-if-feature c) (featurep it) t))
(defmethod action-valid-p (plan (o null) c) (declare (ignorable plan o c)) nil)
(defmethod action-valid-p (plan o (c null)) (declare (ignorable plan o c)) nil)
(defmethod action-valid-p ((plan plan-traversal) (o operation) (s system))
  (and (not (action-forced-not-p plan o s)) (call-next-method)))


;;; Computing stamps and traversing the dependency graph of actions.

(defun* visit-dependencies (operation component fun &aux stamp)
  (loop :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
        :unless (eq dep-o-spec 'feature) ;; avoid the "FEATURE" misfeature
          :do (loop :with dep-o = (find-operation operation dep-o-spec)
                    :for dep-c-spec :in dep-c-specs
                    :for dep-c = (resolve-dependency-spec component dep-c-spec)
                    :do (latest-stamp-f stamp (funcall fun dep-o dep-c))))
  stamp)

(defmethod compute-action-stamp (plan (o operation) (c component) &key just-done)
  ;; In a distant future, safe-file-write-date and component-operation-time
  ;; shall also be parametrized by the plan, or by a second model object.
  (let* ((stamp-lookup #'(lambda (o c) (aif (plan-action-status plan o c) (action-stamp it) t)))
         (out-files (output-files o c))
         (in-files (input-files o c))
         ;; Three kinds of actions:
         (out-op (and out-files t)) ; those that create files on the filesystem
         ;(image-op (and in-files (null out-files))) ; those that load stuff into the image
         ;(null-op (and (null out-files) (null in-files))) ; dependency placeholders that do nothing
         ;; When was the thing last actually done? (Now, or ask.)
         (op-time (or just-done (component-operation-time o c)))
         ;; Accumulated timestamp from dependencies (or T if forced or out-of-date)
         (dep-stamp (visit-dependencies o c stamp-lookup))
         ;; Time stamps from the files at hand, and whether any is missing
         (out-stamps (mapcar #'safe-file-write-date out-files))
         (in-stamps (mapcar #'safe-file-write-date in-files))
         (missing-in
           (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
         (missing-out
           (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
         (all-present (not (or missing-in missing-out)))
         ;; Has any input changed since we last generated the files?
         (earliest-out (stamps-earliest out-stamps))
         (latest-in (stamps-latest (cons dep-stamp in-stamps)))
         (up-to-date-p (stamp<= latest-in earliest-out))
         ;; If everything is up to date, the latest of inputs and outputs is our stamp
         (done-stamp (stamps-latest (cons latest-in out-stamps))))
    ;; Warn if some files are missing:
    ;; either our model is wrong or some other process is messing with our files.
    (when (and just-done (not all-present))
      (warn "~A completed without ~:[~*~;~*its input file~:p~2:*~{ ~S~}~*~]~
             ~:[~; or ~]~:[~*~;~*its output file~:p~2:*~{ ~S~}~*~]"
            (operation-description o c)
            missing-in (length missing-in) (and missing-in missing-out)
            missing-out (length missing-out)))
    ;; Note that we use stamp<= instead of stamp< to play nice with generated files.
    ;; Any race condition is intrinsic to the limited timestamp resolution.
    (if (or just-done ;; The done-stamp is valid: if we're just done, or
            ;; if all filesystem effects are up-to-date and there's no invalidating reason.
            (and all-present up-to-date-p (operation-done-p o c) (not (action-forced-p plan o c))))
        (values done-stamp ;; return the hard-earned timestamp
                (or just-done
                    (or out-op ;; a file-creating op is done when all files are up to date
                        ;; a image-effecting a placeholder op is done when it was actually run,
                        (and op-time (eql op-time done-stamp))))) ;; with the matching stamp
        ;; done-stamp invalid: return a timestamp in an indefinite future, action not done yet
        (values t nil))))

(defgeneric call-while-visiting-action (plan operation component function)
  (:documentation "Detect circular dependencies"))
(defmethod call-while-visiting-action ((plan plan-traversal) operation component fun)
  (with-accessors ((action-set plan-visiting-action-set)
                   (action-list plan-visiting-action-list)) plan
    (let ((action (cons operation component)))
      (aif (gethash action action-set)
           (error 'circular-dependency :actions
                  (member action (reverse action-list) :test 'equal))
           (progn
             (setf (gethash action action-set) t)
             (push action action-list)
             (unwind-protect
                  (funcall fun)
               (pop action-list)
               (setf (gethash action action-set) nil)))))))

(defmacro while-visiting-action ((p o c) &body body)
  `(call-while-visiting-action ,p ,o ,c #'(lambda () ,@body)))

(defgeneric* traverse-action (plan operation component needed-in-image-p))

(defmethod (setf plan-action-status) :after
    (new-status (p sequential-plan) (o operation) (c component))
  (when (action-planned-p new-status)
    (push (cons o c) (plan-actions-r p))))

(defmethod plan-record-dependency ((plan sequential-plan)
                                   (operation operation) (component component))
  (declare (ignorable plan operation component))
  (values))

(defmethod traverse-action (plan operation component needed-in-image-p)
  (block nil
    (unless (action-valid-p plan operation component) (return nil))
    (plan-record-dependency plan operation component)
    (let* ((aniip (needed-in-image-p operation component))
           (eniip (and aniip needed-in-image-p))
           (status (plan-action-status plan operation component)))
      (when (and status (or (action-done-p status) (action-planned-p status) (not eniip)))
        ;; Already visited with sufficient need-in-image level: just return the stamp.
        (return (action-stamp status)))
      (labels ((visit-action (niip)
                 (visit-dependencies operation component
                                     #'(lambda (o c) (traverse-action plan o c niip)))
                 (multiple-value-bind (stamp done-p)
                     (compute-action-stamp plan operation component)
                   (let ((add-to-plan-p (or (eql stamp t) (and niip (not done-p)))))
                     (cond
                       ((and add-to-plan-p (not niip)) ;; if we need to do it,
                        (visit-action t)) ;; then we need to do it in the image!
                       (t
                        (setf (plan-action-status plan operation component)
                              (make-instance
                               'planned-action-status
                               :stamp stamp
                               :done-p (and done-p (not add-to-plan-p))
                               :planned-p add-to-plan-p
                               :index (if status (action-index status) (incf (plan-total-action-count plan)))))
                        (when add-to-plan-p
                          (incf (plan-planned-action-count plan))
                          (unless aniip
                            (incf (plan-planned-output-action-count plan))))
                        stamp))))))
        (while-visiting-action (plan operation component) ; maintain context, handle circularity.
          (visit-action eniip))))))

(defun* traverse-sequentially (operation component &rest keys &key force force-not verbose)
  (declare (ignore force force-not verbose))
  (let ((plan (apply 'make-instance 'sequential-plan :system (component-system component) keys)))
    (traverse-action plan operation component t)
    (reverse (plan-actions-r plan))))

(defmethod traverse ((o operation) (c component) &rest keys &key force force-not verbose)
  (declare (ignore force force-not verbose))
  (apply 'traverse-sequentially o c keys))

(defmethod perform ((o operation) (c source-file))
  (sysdef-error
   (compatfmt "~@<Required method PERFORM not implemented for operation ~A, component ~A~@:>")
   (class-of o) (class-of c)))

(defmethod perform ((o operation) (c parent-component))
  (declare (ignorable o c))
  nil)

(defmethod perform-with-restarts (operation component)
  ;; TOO verbose, especially as the default. Add your own :before method
  ;; to perform-with-restart or perform if you want that:
  #|(when *asdf-verbose* (explain operation component))|#
  (perform operation component))

(defmethod perform-with-restarts :around (operation component)
  (loop
    (restart-case
        (return (call-next-method))
      (retry ()
        :report
        (lambda (s)
          (format s (compatfmt "~@<Retry ~A.~@:>")
                  (operation-description operation component))))
      (accept ()
        :report
        (lambda (s)
          (format s (compatfmt "~@<Continue, treating ~A as having been successful.~@:>")
                  (operation-description operation component)))
        (mark-operation-done operation component)
        (return)))))

(defmethod explain ((o operation) (c component))
  (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (operation-description o c)))

(defmethod operation-description (operation component)
  (format nil (compatfmt "~@<~A on ~A~@:>")
          (class-of operation) component))

;;;; -------------------------------------------------------------------------
;;;; compile-op

(defclass compile-op (downward-operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (flags :initarg :flags :accessor compile-op-flags
          :initform nil)))

;; backward-compatibility
(defgeneric* operation-on-warnings (operation))
(defmethod operation-on-warnings ((o operation))
  (declare (ignorable o)) *compile-file-warnings-behaviour*)
(defgeneric* operation-on-failure (operation))
(defmethod operation-on-failure ((o operation))
  (declare (ignorable o)) *compile-file-failure-behaviour*)

(defun* ensure-all-directories-exist (pathnames)
   (dolist (pathname pathnames)
     (ensure-directories-exist (translate-logical-pathname pathname))))

(defmethod perform :before ((o operation) (c component))
  (ensure-all-directories-exist (output-files o c)))

(defgeneric* around-compile-hook (component))
(defgeneric* call-with-around-compile-hook (component thunk))

(defmethod around-compile-hook ((c component))
  (cond
    ((slot-boundp c 'around-compile)
     (slot-value c 'around-compile))
    ((component-parent c)
     (around-compile-hook (component-parent c)))))

(defun* ensure-function (fun &key (package :asdf))
  (etypecase fun
    ((or symbol function) fun)
    (cons (eval `(function ,fun)))
    (string (eval `(function ,(with-standard-io-syntax
                               (let ((*package* (find-package package)))
                                 (read-from-string fun))))))))

(defun* call-around-hook (hook function)
  (funcall (or (ensure-function hook) 'funcall) function))

(defmethod call-with-around-compile-hook ((c component) function)
  (call-around-hook (around-compile-hook c) function))

;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((o compile-op) (c cl-source-file))
  (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
        ;; we consult input-files, the first of which should be the one to compile-file
        (input-file (first (input-files o c))) 
        ;; on some implementations, there are more than one output-file,
        ;; but the first one should always be the primary fasl that gets loaded.
        (output-file (first (output-files o c))))
    (multiple-value-bind (output warnings-p failure-p)
        (call-with-around-compile-hook
         c #'(lambda (&rest flags)
               (apply *compile-op-compile-file-function* input-file
                      :output-file output-file
                      #-gcl<2.7 :external-format #-gcl<2.7 (component-external-format c)
                      (append flags (compile-op-flags o)))))
      (unless output
        (error 'compile-error :component c :operation o))
      (when failure-p
        (case *compile-file-failure-behaviour*
          (:warn (warn
                  (compatfmt "~@<COMPILE-FILE failed while performing ~A.~@:>")
                  (operation-description o c)))
          (:error (error 'compile-failed :component c :operation o))
          (:ignore nil)))
      (when warnings-p
        (case *compile-file-warnings-behaviour*
          (:warn (warn
                  (compatfmt "~@<COMPILE-FILE warned while performing ~A.~@:>")
                  (operation-description o c)))
          (:error (error 'compile-warned :component c :operation o))
          (:ignore nil))))))

(defmethod output-files ((o compile-op) (c cl-source-file))
  (declare (ignorable o))
  (let* ((p (lispize-pathname (component-pathname c)))
         (f (compile-file-pathname ;; fasl
             p #+mkcl :fasl-p #+mkcl t #+ecl :type #+ecl :fasl))
         #+mkcl (o (compile-file-pathname p :fasl-p nil))) ;; object file
    #+ecl (if (use-ecl-byte-compiler-p)
              (list f)
              (list (compile-file-pathname p :type :object) f))
    #+mkcl (list o f)
    #-(or ecl mkcl) (list f)))

(defmethod component-depends-on ((o compile-op) (c component))
  (declare (ignorable o))
  `((prepare-op ,c) ,@(call-next-method)))

(defmethod perform ((o compile-op) (c static-file))
  (declare (ignorable o c))
  nil)

(defmethod output-files ((o compile-op) (c static-file))
  (declare (ignorable o c))
  nil)

(defmethod operation-description ((o compile-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<compiling ~3i~_~A~@:>") c))

(defmethod operation-description ((o compile-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<compiled ~3i~_~A~@:>") c))


;;;; -------------------------------------------------------------------------
;;;; load-op

(defmethod perform-with-restarts ((o load-op) (c cl-source-file))
  (loop
    (restart-case
        (return (call-next-method))
      (try-recompiling ()
        :report (lambda (s)
                  (format s "Recompile ~a and try loading it again"
                          (component-name c)))
        (perform (find-operation o 'compile-op) c)))))

(defmethod perform ((o load-op) (c cl-source-file))
  (map () #'load
       #-(or ecl mkcl)
       (input-files o c)
       #+(or ecl mkcl)
       (loop :for i :in (input-files o c)
             :unless (string= (pathname-type i) "fas")
             :collect (compile-file-pathname (lispize-pathname i)))))

(defmethod perform ((o load-op) (c static-file))
  (declare (ignorable o c))
  nil)

(defmethod component-depends-on ((o load-op) (c component))
  (declare (ignorable o))
  `((prepare-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((o load-op) (c source-file))
  (declare (ignorable o))
  `((compile-op ,c) ,@(call-next-method)))

(defmethod operation-description ((o load-op) component)
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading ~3i~_~A~@:>")
          component))

(defmethod operation-description ((o load-op) (c cl-source-file))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading FASL for ~3i~_~A~@:>") c))

(defmethod operation-description ((o load-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loaded ~3i~_~A~@:>") c))

(defmethod operation-description ((o prepare-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading dependencies of ~3i~_~A~@:>") c))


;;;; -------------------------------------------------------------------------
;;;; load-source-op

(defclass load-source-op (basic-load-op downward-operation) ())
(defclass prepare-source-op (upward-operation) ())

(defmethod component-depends-on ((o prepare-source-op) (c source-file))
  (declare (ignorable o))
  `((load-source-op ,@(component-sibling-dependencies c)) ,@(call-next-method)))
(defmethod input-files ((o prepare-source-op) (c component))
  (declare (ignorable o c))
  nil)
(defmethod input-files ((o prepare-source-op) (s system))
  (declare (ignorable o))
  (aif (system-source-file s) (list it)))
(defmethod perform ((o prepare-source-op) (c component))
  (declare (ignorable o c))
  nil)

(defmethod component-depends-on ((o load-source-op) (c component))
  (declare (ignorable o))
  `((prepare-source-op ,c) ,@(call-next-method)))

(defmethod perform ((o load-source-op) (c cl-source-file))
  (declare (ignorable o))
  (call-with-around-compile-hook
   c #'(lambda () (load (component-pathname c)
                        #-gcl<2.7 :external-format #-gcl<2.7 (component-external-format c)))))

(defmethod perform ((o load-source-op) (c static-file))
  (declare (ignorable o c))
  nil)

(defmethod output-files ((o load-source-op) (c component))
  (declare (ignorable o c))
  nil)

(defmethod operation-description ((o load-source-op) c)
  (declare (ignorable o))
  (format nil (compatfmt "~@<Loading source of ~3i~_~A~@:>") c))

(defmethod operation-description ((o load-source-op) (c parent-component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<Loaded source of ~3i~_~A~@:>") c))

(defmethod operation-description ((o prepare-source-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<loading source for dependencies of ~3i~_~A~@:>") c))


;;;; -------------------------------------------------------------------------
;;;; test-op

(defclass test-op (operation) ())

(defmethod perform ((o test-op) (c component))
  (declare (ignorable o c))
  nil)

(defmethod operation-done-p ((o test-op) (c system))
  "Testing a system is _never_ done."
  (declare (ignorable o c))
  nil)

(defmethod component-depends-on ((o test-op) (c system))
  (declare (ignorable o))
  `((load-op ,c) ,@(call-next-method)))


;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(defgeneric* operate (operation-class system &key &allow-other-keys))
(defgeneric* perform-plan (plan &key))
(defgeneric* plan-operates-on-p (plan component))

(defvar *post-upgrade-hook* ())

;;;; Separating this into a different function makes it more forward-compatible
(defun* cleanup-upgraded-asdf (old-version)
  (let ((new-version (asdf-version)))
    (unless (equal old-version new-version)
      (cond
        ((version-satisfies new-version old-version)
         (asdf-message (compatfmt "~&~@<; ~@;Upgraded ASDF from version ~A to version ~A~@:>~%")
                       old-version new-version))
        ((version-satisfies old-version new-version)
         (warn (compatfmt "~&~@<; ~@;Downgraded ASDF from version ~A to version ~A~@:>~%")
               old-version new-version))
        (t
         (asdf-message (compatfmt "~&~@<; ~@;Changed ASDF from version ~A to incompatible version ~A~@:>~%")
                       old-version new-version)))
      (let ((asdf (funcall (find-symbol* 'find-system :asdf) :asdf)))
        ;; Invalidate all systems but ASDF itself.
        (setf *defined-systems* (make-defined-systems-table))
        (register-system asdf)
        (funcall (find-symbol* 'load-system :asdf) :asdf) ;; load ASDF a second time, the right way.
        (map () 'funcall *post-upgrade-hook*)
        ;; If we're in the middle of something, restart it.
        (when *systems-being-defined*
          (let ((l (loop :for name :being :the :hash-keys :of *systems-being-defined* :collect name)))
            (clrhash *systems-being-defined*)
            (dolist (s l) (find-system s nil))))
        t))))

;;;; Try to upgrade of ASDF. If a different version was used, return T.
;;;; We need do that before we operate on anything that depends on ASDF.
(defun* upgrade-asdf ()
  (let ((version (asdf-version)))
    (handler-bind (((or style-warning warning) #'muffle-warning))
      (operate 'load-op :asdf :verbose nil))
    (cleanup-upgraded-asdf version)))

(defmethod plan-operates-on-p ((plan list) (component-path list))
  (find component-path (mapcar 'cdr plan)
        :test 'equal :key 'component-find-path))

(defmethod perform-plan ((steps list) &key)
  (let ((*package* *package*)
        (*readtable* *readtable*))
    (with-compilation-unit ()
      (loop :for (op . component) :in steps :do
        (perform-with-restarts op component)))))

(defmethod operate :around (operation-class system
                            &key verbose
                              (on-warnings *compile-file-warnings-behaviour*)
                              (on-failure *compile-file-failure-behaviour*) &allow-other-keys)
  (declare (ignorable operation-class system))
  (with-system-definitions ()
    (let* ((*asdf-verbose* verbose)
           (*verbose-out* (if verbose *standard-output* (make-broadcast-stream)))
           (*compile-file-warnings-behaviour* on-warnings)
           (*compile-file-failure-behaviour* on-failure))
      (call-next-method))))

(defmethod operate (operation-class system &rest args &key version &allow-other-keys)
  (let* ((system (etypecase system
                   (system system)
                   ((or string symbol) (find-system system))))
         ;; I'd like to remove-keys :force :force-not :verbose, but swank.asd relies on :force (!).
         (op (apply 'make-operation operation-class args))
         (systems-being-operated *systems-being-operated*)
         (*systems-being-operated* (or systems-being-operated (make-hash-table :test 'equal))))
    (check-type system system)
    (setf (gethash (coerce-name system) *systems-being-operated*) system)
    (flet ((upgrade ()
             ;; If we needed to upgrade ASDF to achieve our goal,
             ;; then do it specially as the first thing,
             ;; which will invalidate all existing systems;
             ;; afterwards, try again with the new OPERATE function,
             ;; which on some implementations may be a new symbol.
             (unless (gethash "asdf" *systems-being-operated*)
               (upgrade-asdf)
               (return-from operate
                 (apply (find-symbol* 'operate :asdf) operation-class system args)))))
      (when systems-being-operated ;; Upgrade if loading a system from another one.
        (upgrade))
      (unless (version-satisfies system version)
        (error 'missing-component-of-version :requires system :version version))
      (let ((plan (apply 'traverse op system args)))
        (when (plan-operates-on-p plan '("asdf"))
          (upgrade)) ;; Upgrade early if the plan involves upgrading asdf at any time.
        (perform-plan plan)
        (values op plan)))))

(defun* oos (operation-class system &rest args
             &key force force-not verbose version &allow-other-keys)
  (declare (ignore force force-not verbose version))
  (apply 'operate operation-class system args))

(let ((operate-docstring
  "Operate does three things:

1. It creates an instance of OPERATION-CLASS using any keyword parameters as initargs.
2. It finds the  asdf-system specified by SYSTEM (possibly loading it from disk).
3. It then calls TRAVERSE with the operation and system as arguments

The traverse operation is wrapped in WITH-COMPILATION-UNIT and error handling code.
If a VERSION argument is supplied, then operate also ensures that the system found
satisfies it using the VERSION-SATISFIES method.

Note that dependencies may cause the operation to invoke other operations on the system
or its components: the new operations will be created with the same initargs as the original one.

The :FORCE or :FORCE-NOT argument to OPERATE can be:
T to force the inside of the specified system to be rebuilt (resp. not),
  without recursively forcing the other systems we depend on.
:ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
(SYSTEM1 SYSTEM2 ... SYSTEMN) to force systems named in a given list
:FORCE has precedence over :FORCE-NOT.
"))
  (setf (documentation 'oos 'function)
        (format nil
                "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
                operate-docstring))
  (setf (documentation 'operate 'function)
        operate-docstring))

(defun* load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'operate *load-system-operation* system keys)
  t)

(defun* load-systems (&rest systems)
  (map () 'load-system systems))

(defun* component-loaded-p (c)
  (action-already-done-p nil (make-instance 'load-op) (find-component c ())))

(defun* already-loaded-systems ()
  (remove-if-not 'component-loaded-p (registered-systems)))

(defun* require-system (s &rest keys &key &allow-other-keys)
  (apply 'load-system s :force-not (already-loaded-systems) keys))

(defun* compile-system (system &rest args &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(asdf:operate 'asdf:compile-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'operate 'compile-op system args)
  t)

(defun* test-system (system &rest args &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(asdf:operate 'asdf:test-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'operate 'test-op system args)
  t)

;;;; -------------------------------------------------------------------------
;;;; Defsystem

(defun* load-pathname ()
  (resolve-symlinks* (or *load-pathname* *compile-file-pathname*)))

(defun* determine-system-pathname (pathname)
  ;; The defsystem macro calls us to determine
  ;; the pathname of a system as follows:
  ;; 1. the one supplied,
  ;; 2. derived from *load-pathname* via load-pathname
  ;; 3. taken from the *default-pathname-defaults* via default-directory
  (let* ((file-pathname (load-pathname))
         (directory-pathname (and file-pathname (pathname-directory-pathname file-pathname))))
    (or (and pathname (subpathname directory-pathname pathname :type :directory))
        directory-pathname
        (default-directory))))

(defun* find-class* (x &optional (errorp t) environment)
  (etypecase x
    ((or standard-class built-in-class) x)
    #+gcl<2.7 (keyword nil)
    (symbol (find-class x errorp environment))))

(defun* class-for-type (parent type)
  (or (loop :for symbol :in (list
                             type
                             (find-symbol* type *package*)
                             (find-symbol* type :asdf))
        :for class = (and symbol (find-class* symbol nil))
        :when (and class
                   (#-cormanlisp subtypep #+cormanlisp cl::subclassp
                                 class (find-class* 'component)))
        :return class)
      (and (eq type :file)
           (find-class*
            (or (loop :for p = parent :then (component-parent p) :while p
                      :thereis (module-default-component-class p))
                *default-component-class*) nil))
      (sysdef-error "don't recognize component type ~A" type)))

(defvar *serial-depends-on* nil)

(defun* sysdef-error-component (msg type name value)
  (sysdef-error (strcat msg (compatfmt "~&~@<The value specified for ~(~A~) ~A is ~S~@:>"))
                type name value))

(defun* check-component-input (type name weakly-depends-on
                              depends-on components in-order-to)
  "A partial test of the values of a component."
  (unless (listp depends-on)
    (sysdef-error-component ":depends-on must be a list."
                            type name depends-on))
  (unless (listp weakly-depends-on)
    (sysdef-error-component ":weakly-depends-on must be a list."
                            type name weakly-depends-on))
  (unless (listp components)
    (sysdef-error-component ":components must be NIL or a list of components."
                            type name components))
  (unless (and (listp in-order-to) (listp (car in-order-to)))
    (sysdef-error-component ":in-order-to must be NIL or a list of components."
                            type name in-order-to)))

(defun* %remove-component-inline-methods (component)
  (dolist (name +asdf-methods+)
    (map ()
         ;; this is inefficient as most of the stored
         ;; methods will not be for this particular gf
         ;; But this is hardly performance-critical
         #'(lambda (m)
             (remove-method (symbol-function name) m))
         (component-inline-methods component)))
  ;; clear methods, then add the new ones
  (component-inline-methods component) nil)

(defun* %define-component-inline-methods (ret rest)
  (dolist (name +asdf-methods+)
    (let ((keyword (intern (symbol-name name) :keyword)))
      (loop :for data = rest :then (cddr data)
        :for key = (first data)
        :for value = (second data)
        :while data
        :when (eq key keyword) :do
        (destructuring-bind (op qual (o c) &body body) value
          (pushnew
           (eval `(defmethod ,name ,qual ((,o ,op) (,c (eql ,ret)))
                             ,@body))
           (component-inline-methods ret)))))))

(defun* %refresh-component-inline-methods (component rest)
  (%remove-component-inline-methods component)
  (%define-component-inline-methods component rest))

;; Resolve away the dubious and deprecated option :IF-COMPONENT-DEP-FAILS
;; and its companion ASDF:FEATURE pseudo-dependency.
;; THIS IS ONLY PARTIAL SUPPORT: it won't recurse into dependencies
;; to accumulate feature conditions.
;; Therefore it will accept the SB-ROTATE-BYTE of an old SBCL
;; (older than 1.1.2.20-fe6da9f) but won't suffice to load an old nibbles.
(defun resolve-if-component-dep-fails (if-component-dep-fails component)
  (asdf-message "The system definition for ~S uses deprecated ~
                 ASDF option :IF-COMPONENT-DEP-DAILS. ~
                 Starting with ASDF 2.27, please use :IF-FEATURE instead"
                (coerce-name (component-system component)))
  ;; This only supports the pattern of use of the "feature" seen in the wild
  (check-type component parent-component)
  (check-type if-component-dep-fails (member :fail :ignore :try-next))
  (unless (eq if-component-dep-fails :fail)
    (loop :with o = (make-instance 'compile-op)
      :for c :in (component-children component) :do
        (loop :for (feature? feature) :in (component-depends-on o c)
              :when (eq feature? 'feature) :do
                (setf (component-if-feature c) feature)))))

(defun* parse-component-form (parent options)
  (destructuring-bind
        (type name &rest rest &key
              ;; the following list of keywords is reproduced below in the
              ;; remove-keys form.  important to keep them in sync
              components pathname
              perform explain output-files operation-done-p
              weakly-depends-on depends-on serial in-order-to
              do-first if-component-dep-fails
              (version nil versionp)
              ;; list ends
              &allow-other-keys) options
    (declare (ignorable perform explain output-files operation-done-p))
    (check-component-input type name weakly-depends-on depends-on components in-order-to)
    (when (and parent
               (find-component parent name)
               (not ;; ignore the same object when rereading the defsystem
                (typep (find-component parent name)
                       (class-for-type parent type))))
      (error 'duplicate-names :name name))
    (when versionp
      (unless (parse-version version nil)
        (warn (compatfmt "~@<Invalid version ~S for component ~S~@[ of ~S~]~@:>")
              version name parent)))
    (when do-first (error "DO-FIRST is not supported anymore since ASDF 2.27"))
    (let* ((args `(:name ,(coerce-name name)
                   :pathname ,pathname
                   ,@(when parent `(:parent ,parent))
                   ,@(remove-keys
                      '(components pathname if-component-dep-fails
                        perform explain output-files operation-done-p
                        weakly-depends-on depends-on serial in-order-to)
                      rest)))
           (ret (find-component parent name)))
      (when weakly-depends-on
        (appendf depends-on (remove-if (complement #'(lambda (x) (find-system x nil))) weakly-depends-on)))
      (when *serial-depends-on*
        (push *serial-depends-on* depends-on))
      (if ret ; preserve identity
          (apply 'reinitialize-instance ret args)
          (setf ret (apply 'make-instance (class-for-type parent type) args)))
      (component-pathname ret) ; eagerly compute the absolute pathname
      (when (typep ret 'parent-component)
        (let ((*serial-depends-on* nil))
          (setf (component-children ret)
                (loop
                  :for c-form :in components
                  :for c = (parse-component-form ret c-form)
                  :for name = (component-name c)
                  :collect c
                  :when serial :do (setf *serial-depends-on* name))))
        (compute-children-by-name ret))
      (setf (component-sibling-dependencies ret) depends-on) ;; Used by POIU. ASDF3: rename to component-depends-on
      (setf (component-in-order-to ret) in-order-to)
      (%refresh-component-inline-methods ret rest)
      (when if-component-dep-fails (resolve-if-component-dep-fails if-component-dep-fails ret))
      ret)))

(defun* reset-system (system &rest keys &key &allow-other-keys)
  (change-class (change-class system 'proto-system) 'system)
  (apply 'reinitialize-instance system keys))

(defun* do-defsystem (name &rest options
                           &key pathname (class 'system)
                           defsystem-depends-on &allow-other-keys)
  ;; The system must be registered before we parse the body,
  ;; otherwise we recur when trying to find an existing system
  ;; of the same name to reuse options (e.g. pathname) from.
  ;; To avoid infinite recursion in cases where you defsystem a system
  ;; that is registered to a different location to find-system,
  ;; we also need to remember it in a special variable *systems-being-defined*.
  (with-system-definitions ()
    (let* ((name (coerce-name name))
           (load-pathname (load-pathname))
           (registered (system-registered-p name))
           (registered! (if registered
                            (rplaca registered (safe-file-write-date load-pathname))
                            (register-system (make-instance 'system :name name :source-file load-pathname))))
           (system (reset-system (cdr registered!)
                                :name name :source-file load-pathname))
           (component-options (remove-keys '(:class) options)))
      (setf (gethash name *systems-being-defined*) system)
      (apply 'load-systems defsystem-depends-on)
      ;; We change-class (when necessary) AFTER we load the defsystem-dep's
      ;; since the class might be defined as part of those.
      (let ((class (class-for-type nil class)))
        (unless (eq (type-of system) class)
          (change-class system class)))
      (parse-component-form
       nil (list*
            :module name
            :pathname (determine-system-pathname pathname)
            component-options)))))

(defmacro defsystem (name &body options)
  `(apply 'do-defsystem ',name ',options))

;;;; ---------------------------------------------------------------------------
;;;; run-shell-command
;;;;
;;;; run-shell-command functions for other lisp implementations will be
;;;; gratefully accepted, if they do the same thing.
;;;; If the docstring is ambiguous, send a bug report.
;;;;
;;;; WARNING! The function below is mostly dysfunctional.
;;;; For instance, it will probably run fine on most implementations on Unix,
;;;; which will hopefully use the shell /bin/sh (which we force in some cases)
;;;; which is hopefully reasonably compatible with a POSIX *or* Bourne shell.
;;;; But behavior on Windows may vary wildly between implementations,
;;;; either relying on your having installed a POSIX sh, or going through
;;;; the CMD.EXE interpreter, for a totally different meaning, depending on
;;;; what is easily expressible in said implementation.
;;;;
;;;; We probably should move this functionality to its own system and deprecate
;;;; use of it from the asdf package. However, this would break unspecified
;;;; existing software, so until a clear alternative exists, we can't deprecate
;;;; it, and even after it's been deprecated, we will support it for a few
;;;; years so everyone has time to migrate away from it. -- fare 2009-12-01
;;;;
;;;; As a suggested replacement which is portable to all ASDF-supported
;;;; implementations and operating systems except Genera, I recommend
;;;; xcvb-driver's xcvb-driver:run-program/ and its derivatives.

(defun* run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (let ((command (apply 'format nil control-string args)))
    (asdf-message "; $ ~A~%" command)

    #+abcl
    (ext:run-shell-command command :output *verbose-out*)

    #+allegro
    ;; will this fail if command has embedded quotes - it seems to work
    (multiple-value-bind (stdout stderr exit-code)
        (excl.osi:command-output
         #-mswindows (vector "/bin/sh" "/bin/sh" "-c" command)
         #+mswindows command ; BEWARE!
         :input nil :whole nil
         #+mswindows :show-window #+mswindows :hide)
      (asdf-message "~{~&~a~%~}~%" stderr)
      (asdf-message "~{~&~a~%~}~%" stdout)
      exit-code)

    #+clisp
    ;; CLISP returns NIL for exit status zero.
    (if *verbose-out*
        (let* ((new-command (format nil "( ~A ) ; r=$? ; echo ; echo ASDF-EXIT-STATUS $r"
                                    command))
               (outstream (ext:run-shell-command new-command :output :stream :wait t)))
            (multiple-value-bind (retval out-lines)
                (unwind-protect
                     (parse-clisp-shell-output outstream)
                  (ignore-errors (close outstream)))
              (asdf-message "~{~&~a~%~}~%" out-lines)
              retval))
        ;; there will be no output, just grab up the exit status
        (or (ext:run-shell-command command :output nil :wait t) 0))

    #+clozure
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program
                 (cond
                   ((os-unix-p) "/bin/sh")
                   ((os-windows-p) (strcat "CMD /C " command)) ; BEWARE!
                   (t (error "Unsupported OS")))
                 (if (os-unix-p) (list "-c" command) '())
                 :input nil :output *verbose-out* :wait t)))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list "-c" command)
      :input nil :output *verbose-out*))

    #+cormanlisp
    (win32:system command)

    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (ext:system command)

    #+gcl
    (lisp:system command)

    #+lispworks
    (apply 'system:call-system-showing-output command
           :show-cmd nil :prefix "" :output-stream *verbose-out*
           (when (os-unix-p) '(:shell-type "/bin/sh")))

    #+mcl
    (ccl::with-cstrs ((%command command)) (_system %command))

    #+mkcl
    ;; This has next to no chance of working on basic Windows!
    ;; Your best hope is that Cygwin or MSYS is somewhere in the PATH.
    (multiple-value-bind (io process exit-code)
        (apply #'mkcl:run-program #+windows "sh" #-windows "/bin/sh"
                                  (list "-c" command)
                                  :input nil :output t #|*verbose-out*|# ;; will be *verbose-out* when we support it
                                  #-windows '(:search nil))
      (declare (ignore io process))
      exit-code)

    #+sbcl
    (sb-ext:process-exit-code
     (apply 'sb-ext:run-program
            #+win32 "sh" #-win32 "/bin/sh"
            (list  "-c" command)
            :input nil :output *verbose-out*
            #+win32 '(:search t) #-win32 nil))

    #+xcl
    (ext:run-shell-command command)

    #-(or abcl allegro clisp clozure cmu ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (error "RUN-SHELL-COMMAND not implemented for this Lisp")))

#+clisp
(defun* parse-clisp-shell-output (stream)
  "Helper function for running shell commands under clisp.  Parses a specially-
crafted output string to recover the exit status of the shell command and a
list of lines of output."
  (loop :with status-prefix = "ASDF-EXIT-STATUS "
    :with prefix-length = (length status-prefix)
    :with exit-status = -1 :with lines = ()
    :for line = (read-line stream nil nil)
    :while line :do (push line lines) :finally
    (let* ((last (car lines))
           (status (and last (>= (length last) prefix-length)
                        (string-equal last status-prefix :end1 prefix-length)
                        (parse-integer last :start prefix-length :junk-allowed t))))
      (when status
        (setf exit-status status)
        (pop lines) (when (equal "" (car lines)) (pop lines)))
      (return (values exit-status (reverse lines))))))

;;;; ---------------------------------------------------------------------------
;;;; system-relative-pathname

(defun* system-definition-pathname (x)
  ;; As of 2.014.8, we mean to make this function obsolete,
  ;; but that won't happen until all clients have been updated.
  ;;(cerror "Use ASDF:SYSTEM-SOURCE-FILE instead"
  "Function ASDF:SYSTEM-DEFINITION-PATHNAME is obsolete.
It used to expose ASDF internals with subtle differences with respect to
user expectations, that have been refactored away since.
We recommend you use ASDF:SYSTEM-SOURCE-FILE instead
for a mostly compatible replacement that we're supporting,
or even ASDF:SYSTEM-SOURCE-DIRECTORY or ASDF:SYSTEM-RELATIVE-PATHNAME
if that's whay you mean." ;;)
  (system-source-file x))

(defmethod system-source-file ((system system))
  ;; might be missing when upgrading from ASDF 1 and u-i-f-r-c failed
  (unless (slot-boundp system 'source-file)
    (%set-system-source-file
     (probe-asd (component-name system) (component-pathname system)) system))
  (slot-value system 'source-file))
(defmethod system-source-file ((system-name string))
  (system-source-file (find-system system-name)))
(defmethod system-source-file ((system-name symbol))
  (system-source-file (find-system system-name)))

(defun* system-source-directory (system-designator)
  "Return a pathname object corresponding to the
directory in which the system specification (.asd file) is
located."
  (pathname-directory-pathname (system-source-file system-designator)))

(defun* relativize-directory (directory)
  (let ((directory (normalize-pathname-directory-component directory)))
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
     :directory (relativize-directory (pathname-directory p))
     :defaults p)))

(defun* system-relative-pathname (system name &key type)
  (subpathname (system-source-directory system) name :type type))


;;; ---------------------------------------------------------------------------
;;; implementation-identifier
;;;
;;; produce a string to identify current implementation.
;;; Initially stolen from SLIME's SWANK, rewritten since.
;;; We're back to runtime checking, for the sake of e.g. ABCL.

(defun* first-feature (features)
  (dolist (x features)
    (multiple-value-bind (val feature)
        (if (consp x) (values (first x) (cons :or (rest x))) (values x x))
      (when (featurep feature) (return val)))))

(defun* implementation-type ()
  (first-feature
   '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp) :cmu
     :ecl :gcl (:lw :lispworks) :mcl :mkcl :sbcl :scl :symbolics :xcl)))

(defun* operating-system ()
  (first-feature
   '(:cygwin (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
     (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
     (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
     (:solaris :solaris :sunos) (:bsd :bsd :freebsd :netbsd :openbsd) :unix
     :genera)))

(defun* architecture ()
  (first-feature
   '((:x64 :amd64 :x86-64 :x86_64 :x8664-target (:and :word-size=64 :pc386))
     (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
     (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
     :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
     :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
     ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
     ;; we may have to segregate the code still by architecture.
     (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

#+clozure
(defun* ccl-fasl-version ()
  ;; the fasl version is target-dependent from CCL 1.8 on.
  (or (let ((s 'ccl::target-fasl-version))
        (and (fboundp s) (funcall s)))
      (and (boundp 'ccl::fasl-version)
           (symbol-value 'ccl::fasl-version))
      (error "Can't determine fasl version.")))

(defun* lisp-version-string ()
  (let ((s (lisp-implementation-version)))
    (car ; as opposed to OR, this idiom prevents some unreachable code warning
     (list
      #+allegro
      (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
              excl::*common-lisp-version-number*
              ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
              (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
              ;; Note if not using International ACL
              ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
              (excl:ics-target-case (:-ics "8"))
              (and (member :smp *features*) "S"))
      #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
      #+clisp
      (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
      #+clozure
      (format nil "~d.~d-f~d" ; shorten for windows
              ccl::*openmcl-major-version*
              ccl::*openmcl-minor-version*
              (logand (ccl-fasl-version) #xFF))
      #+cmu (substitute #\- #\/ s)
      #+scl (format nil "~A~A" s
                    ;; ANSI upper case vs lower case.
                    (ecase ext:*case-mode* (:upper "") (:lower "l")))
      #+ecl (format nil "~A~@[-~A~]" s
                    (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                      (subseq vcs-id 0 (min (length vcs-id) 8))))
      #+gcl (subseq s (1+ (position #\space s)))
      #+genera
      (multiple-value-bind (major minor) (sct:get-system-version "System")
        (format nil "~D.~D" major minor))
      #+mcl (subseq s 8) ; strip the leading "Version "
      s))))

(defun* implementation-identifier ()
  (substitute-if
   #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
   (format nil "~(~a~@{~@[-~a~]~}~)"
           (or (implementation-type) (lisp-implementation-type))
           (or (lisp-version-string) (lisp-implementation-version))
           (or (operating-system) (software-type))
           (or (architecture) (machine-type)))))

(defun* hostname ()
  ;; Note: untested on RMCL
  #+(or abcl clozure cmucl ecl genera lispworks mcl mkcl sbcl scl xcl) (machine-instance)
  #+cormanlisp "localhost" ;; is there a better way? Does it matter?
  #+allegro (excl.osi:gethostname)
  #+clisp (first (split-string (machine-instance) :separator " "))
  #+gcl (system:gethostname))


;;; ---------------------------------------------------------------------------
;;; Generic support for configuration files

(defun* inter-directory-separator ()
  (if (os-unix-p) #\: #\;))

(defun* user-homedir ()
  (truenamize
   (pathname-directory-pathname
    #+cormanlisp (ensure-directory-pathname (user-homedir-pathname))
    #+mcl (current-user-homedir-pathname)
    #-(or cormanlisp mcl) (user-homedir-pathname))))

(defun* ensure-pathname* (x want-absolute want-directory fmt &rest args)
  (when (plusp (length x))
    (let ((p (if want-directory (ensure-directory-pathname x) (pathname x))))
      (when want-absolute
        (unless (absolute-pathname-p p)
          (cerror "ignore relative pathname"
                  "Invalid relative pathname ~A~@[ ~?~]" x fmt args)
          (return-from ensure-pathname* nil)))
      p)))
(defun* split-pathnames* (x want-absolute want-directory fmt &rest args)
  (loop :for dir :in (split-string
                      x :separator (string (inter-directory-separator)))
        :collect (apply 'ensure-pathname* dir want-absolute want-directory fmt args)))
(defun* getenv-pathname (x &key want-absolute want-directory &aux (s (getenv x)))
  (ensure-pathname* s want-absolute want-directory "from (getenv ~S)" x))
(defun* getenv-pathnames (x &key want-absolute want-directory &aux (s (getenv x)))
  (and (plusp (length s))
       (split-pathnames* s want-absolute want-directory "from (getenv ~S) = ~S" x s)))
(defun* getenv-absolute-directory (x)
  (getenv-pathname x :want-absolute t :want-directory t))
(defun* getenv-absolute-directories (x)
  (getenv-pathnames x :want-absolute t :want-directory t))

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
     (aif (subpathname* (get-folder-path :common-appdata) "common-lisp/config/")
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

(defun* hidden-file-p (pathname)
  (equal (first-char (pathname-name pathname)) #\.))

(defun* directory* (pathname-spec &rest keys &key &allow-other-keys)
  (apply 'directory pathname-spec
         (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                             #+clozure '(:follow-links nil)
                             #+clisp '(:circle t :if-does-not-exist :ignore)
                             #+(or cmu scl) '(:follow-links nil :truenamep nil)
                             #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl)
                                      '(:resolve-symlinks nil))))))

(defun* validate-configuration-directory (directory tag validator &key invalid-form-reporter)
  "Map the VALIDATOR across the .conf files in DIRECTORY, the TAG will
be applied to the results to yield a configuration form.  Current
values of TAG include :source-registry and :output-translations."
  (let ((files (sort (ignore-errors
                       (remove-if
                        'hidden-file-p
                        (directory* (make-pathname :name :wild :type "conf" :defaults directory))))
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


;;; ---------------------------------------------------------------------------
;;; asdf-output-translations
;;;
;;; this code is heavily inspired from
;;; asdf-binary-translations, common-lisp-controller and cl-launch.
;;; ---------------------------------------------------------------------------

(defvar *output-translations* ()
  "Either NIL (for uninitialized), or a list of one element,
said element itself being a sorted list of mappings.
Each mapping is a pair of a source pathname and destination pathname,
and the order is by decreasing length of namestring of the source pathname.")

(defvar *user-cache*
  (flet ((try (x &rest sub) (and x `(,x ,@sub))))
    (or
     (try (getenv-absolute-directory "XDG_CACHE_HOME") "common-lisp" :implementation)
     (when (os-windows-p)
       (try (or (get-folder-path :local-appdata)
                (get-folder-path :appdata))
            "common-lisp" "cache" :implementation))
     '(:home ".cache" "common-lisp" :implementation))))

(defun* output-translations ()
  (car *output-translations*))

(defun* set-output-translations (new-value)
  (setf *output-translations*
        (list
         (stable-sort (copy-list new-value) #'>
                      :key #'(lambda (x)
                               (etypecase (car x)
                                 ((eql t) -1)
                                 (pathname
                                  (let ((directory (pathname-directory (car x))))
                                    (if (listp directory) (length directory) 0))))))))
  new-value)
(defsetf output-translations set-output-translations) ; works with gcl 2.6

(defun* output-translations-initialized-p ()
  (and *output-translations* t))

(defun* clear-output-translations ()
  "Undoes any initialization of the output translations.
You might want to call that before you dump an image that would be resumed
with a different configuration, so the configuration would be re-read then."
  (setf *output-translations* '())
  (values))

(declaim (ftype (function (t &key (:directory boolean) (:wilden boolean))
                          (values (or null pathname) &optional))
                resolve-location))

(defun* resolve-relative-location-component (x &key directory wilden)
  (let ((r (etypecase x
             (pathname x)
             (string (coerce-pathname x :type (when directory :directory)))
             (cons
              (if (null (cdr x))
                  (resolve-relative-location-component
                   (car x) :directory directory :wilden wilden)
                  (let* ((car (resolve-relative-location-component
                               (car x) :directory t :wilden nil)))
                    (merge-pathnames*
                     (resolve-relative-location-component
                      (cdr x) :directory directory :wilden wilden)
                     car))))
             ((eql :default-directory)
              (relativize-pathname-directory (default-directory)))
             ((eql :*/) *wild-directory*)
             ((eql :**/) *wild-inferiors*)
             ((eql :*.*.*) *wild-file*)
             ((eql :implementation)
              (coerce-pathname (implementation-identifier) :type :directory))
             ((eql :implementation-type)
              (coerce-pathname (string-downcase (implementation-type)) :type :directory))
             ((eql :hostname)
              (coerce-pathname (hostname) :type :directory)))))
    (when (absolute-pathname-p r)
      (error (compatfmt "~@<pathname ~S is not relative~@:>") x))
    (if (or (pathnamep x) (not wilden)) r (wilden r))))

(defvar *here-directory* nil
  "This special variable is bound to the currect directory during calls to
PROCESS-SOURCE-REGISTRY in order that we be able to interpret the :here
directive.")

(defun* resolve-absolute-location-component (x &key directory wilden)
  (let* ((r
          (etypecase x
            (pathname x)
            (string (let ((p (#+mcl probe-posix #-mcl parse-namestring x)))
                      #+mcl (unless p (error "POSIX pathname ~S does not exist" x))
                      (if directory (ensure-directory-pathname p) p)))
            (cons
             (return-from resolve-absolute-location-component
               (if (null (cdr x))
                   (resolve-absolute-location-component
                    (car x) :directory directory :wilden wilden)
                   (merge-pathnames*
                    (resolve-relative-location-component
                     (cdr x) :directory directory :wilden wilden)
                    (resolve-absolute-location-component
                     (car x) :directory t :wilden nil)))))
            ((eql :root)
             ;; special magic! we encode such paths as relative pathnames,
             ;; but it means "relative to the root of the source pathname's host and device".
             (return-from resolve-absolute-location-component
               (let ((p (make-pathname* :directory '(:relative))))
                 (if wilden (wilden p) p))))
            ((eql :home) (user-homedir))
            ((eql :here)
             (resolve-location (or *here-directory*
                                   ;; give semantics in the case of use interactively
                                   :default-directory)
                          :directory t :wilden nil))
            ((eql :user-cache) (resolve-location *user-cache* :directory t :wilden nil))
            ((eql :system-cache)
             (error "Using the :system-cache is deprecated. ~%~
Please remove it from your ASDF configuration"))
            ((eql :default-directory) (default-directory))))
         (s (if (and wilden (not (pathnamep x)))
                (wilden r)
                r)))
    (unless (absolute-pathname-p s)
      (error (compatfmt "~@<Invalid designator for an absolute pathname: ~3i~_~S~@:>") x))
    s))

(defun* resolve-location (x &key directory wilden)
  (if (atom x)
      (resolve-absolute-location-component x :directory directory :wilden wilden)
      (loop :with path = (resolve-absolute-location-component
                          (car x) :directory (and (or directory (cdr x)) t)
                          :wilden (and wilden (null (cdr x))))
        :for (component . morep) :on (cdr x)
        :for dir = (and (or morep directory) t)
        :for wild = (and wilden (not morep))
        :do (setf path (merge-pathnames*
                        (resolve-relative-location-component
                         component :directory dir :wilden wild)
                        path))
        :finally (return path))))

(defun* location-designator-p (x)
  (flet ((absolute-component-p (c)
           (typep c '(or string pathname
                      (member :root :home :here :user-cache :system-cache :default-directory))))
         (relative-component-p (c)
           (typep c '(or string pathname
                      (member :default-directory :*/ :**/ :*.*.*
                        :implementation :implementation-type)))))
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

(defun* validate-output-translations-directive (directive)
  (or (member directive '(:enable-user-cache :disable-cache nil))
      (and (consp directive)
           (or (and (length=n-p directive 2)
                    (or (and (eq (first directive) :include)
                             (typep (second directive) '(or string pathname null)))
                        (and (location-designator-p (first directive))
                             (or (location-designator-p (second directive))
                                 (location-function-p (second directive))))))
               (and (length=n-p directive 1)
                    (location-designator-p (first directive)))))))

(defun* validate-output-translations-form (form &key location)
  (validate-configuration-form
   form
   :output-translations
   'validate-output-translations-directive
   :location location :invalid-form-reporter 'invalid-output-translation))

(defun* validate-output-translations-file (file)
  (validate-configuration-file
   file 'validate-output-translations-form :description "output translations"))

(defun* validate-output-translations-directory (directory)
  (validate-configuration-directory
   directory :output-translations 'validate-output-translations-directive
   :invalid-form-reporter 'invalid-output-translation))

(defun* parse-output-translations-string (string &key location)
  (cond
    ((or (null string) (equal string ""))
     '(:output-translations :inherit-configuration))
    ((not (stringp string))
     (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
    ((eql (char string 0) #\")
     (parse-output-translations-string (read-from-string string) :location location))
    ((eql (char string 0) #\()
     (validate-output-translations-form (read-from-string string) :location location))
    (t
     (loop
      :with inherit = nil
      :with directives = ()
      :with start = 0
      :with end = (length string)
      :with source = nil
      :with separator = (inter-directory-separator)
      :for i = (or (position separator string :start start) end) :do
      (let ((s (subseq string start i)))
        (cond
          (source
           (push (list source (if (equal "" s) nil s)) directives)
           (setf source nil))
          ((equal "" s)
           (when inherit
             (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                    string))
           (setf inherit t)
           (push :inherit-configuration directives))
          (t
           (setf source s)))
        (setf start (1+ i))
        (when (> start end)
          (when source
            (error (compatfmt "~@<Uneven number of components in source to destination mapping: ~3i~_~S~@:>")
                   string))
          (unless inherit
            (push :ignore-inherited-configuration directives))
          (return `(:output-translations ,@(nreverse directives)))))))))

(defparameter *default-output-translations*
  '(environment-output-translations
    user-output-translations-pathname
    user-output-translations-directory-pathname
    system-output-translations-pathname
    system-output-translations-directory-pathname))

(defun* lisp-implementation-directory (&key truename)
  (let ((dir
          (ignore-errors
           #+clozure (let ((*default-pathname-defaults* #p"")) (truename #p"ccl:"))
           #+(or ecl mkcl) #p"SYS:"
           #+sbcl (aif (find-symbol* :sbcl-homedir-pathname :sb-int)
                       (funcall it)
                       (getenv-pathname "SBCL_HOME" :want-directory t)))))
    (if (and dir truename)
        (let ((*default-pathname-defaults* #p"")) (truename dir))
        dir)))

(defun* wrapping-output-translations ()
  `(:output-translations
    ;; Some implementations have precompiled ASDF systems,
    ;; so we must disable translations for implementation paths.
    #+(or #|clozure|# ecl mkcl sbcl)
    ,@(let ((h (lisp-implementation-directory :truename t))) (when h `(((,h ,*wild-inferiors*) ()))))
    #+mkcl (,(translate-logical-pathname "CONTRIB:") ())
    ;; All-import, here is where we want user stuff to be:
    :inherit-configuration
    ;; These are for convenience, and can be overridden by the user:
    #+abcl (#p"/___jar___file___root___/**/*.*" (:user-cache #p"**/*.*"))
    #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
    ;; We enable the user cache by default, and here is the place we do:
    :enable-user-cache))

(defparameter *output-translations-file* (coerce-pathname "asdf-output-translations.conf"))
(defparameter *output-translations-directory* (coerce-pathname "asdf-output-translations.conf.d/"))

(defun* user-output-translations-pathname (&key (direction :input))
  (in-user-configuration-directory *output-translations-file* :direction direction))
(defun* system-output-translations-pathname (&key (direction :input))
  (in-system-configuration-directory *output-translations-file* :direction direction))
(defun* user-output-translations-directory-pathname (&key (direction :input))
  (in-user-configuration-directory *output-translations-directory* :direction direction))
(defun* system-output-translations-directory-pathname (&key (direction :input))
  (in-system-configuration-directory *output-translations-directory* :direction direction))
(defun* environment-output-translations ()
  (getenv "ASDF_OUTPUT_TRANSLATIONS"))

(defgeneric* process-output-translations (spec &key inherit collect))
(declaim (ftype (function (t &key (:collect (or symbol function))) t)
                inherit-output-translations))
(declaim (ftype (function (t &key (:collect (or symbol function)) (:inherit list)) t)
                process-output-translations-directive))

(defmethod process-output-translations ((x symbol) &key
                                        (inherit *default-output-translations*)
                                        collect)
  (process-output-translations (funcall x) :inherit inherit :collect collect))
(defmethod process-output-translations ((pathname #-gcl<2.7 pathname #+gcl<2.7 t) &key inherit collect)
  (cond
    ((directory-pathname-p pathname)
     (process-output-translations (validate-output-translations-directory pathname)
                                  :inherit inherit :collect collect))
    ((probe-file* pathname)
     (process-output-translations (validate-output-translations-file pathname)
                                  :inherit inherit :collect collect))
    (t
     (inherit-output-translations inherit :collect collect))))
(defmethod process-output-translations ((string string) &key inherit collect)
  (process-output-translations (parse-output-translations-string string)
                               :inherit inherit :collect collect))
(defmethod process-output-translations ((x null) &key inherit collect)
  (declare (ignorable x))
  (inherit-output-translations inherit :collect collect))
(defmethod process-output-translations ((form cons) &key inherit collect)
  (dolist (directive (cdr (validate-output-translations-form form)))
    (process-output-translations-directive directive :inherit inherit :collect collect)))

(defun* inherit-output-translations (inherit &key collect)
  (when inherit
    (process-output-translations (first inherit) :collect collect :inherit (rest inherit))))

(defun* process-output-translations-directive (directive &key inherit collect)
  (if (atom directive)
      (ecase directive
        ((:enable-user-cache)
         (process-output-translations-directive '(t :user-cache) :collect collect))
        ((:disable-cache)
         (process-output-translations-directive '(t t) :collect collect))
        ((:inherit-configuration)
         (inherit-output-translations inherit :collect collect))
        ((:ignore-inherited-configuration :ignore-invalid-entries nil)
         nil))
      (let ((src (first directive))
            (dst (second directive)))
        (if (eq src :include)
            (when dst
              (process-output-translations (pathname dst) :inherit nil :collect collect))
            (when src
              (let ((trusrc (or (eql src t)
                                (let ((loc (resolve-location src :directory t :wilden t)))
                                  (if (absolute-pathname-p loc) (truenamize loc) loc)))))
                (cond
                  ((location-function-p dst)
                   (funcall collect
                            (list trusrc
                                  (if (symbolp (second dst))
                                      (fdefinition (second dst))
                                      (eval (second dst))))))
                  ((eq dst t)
                   (funcall collect (list trusrc t)))
                  (t
                   (let* ((trudst (if dst
                                      (resolve-location dst :directory t :wilden t)
                                      trusrc))
                          (wilddst (merge-pathnames* *wild-file* trudst)))
                     (funcall collect (list wilddst t))
                     (funcall collect (list trusrc trudst)))))))))))

(defun* compute-output-translations (&optional parameter)
  "read the configuration, return it"
  (remove-duplicates
   (while-collecting (c)
     (inherit-output-translations
      `(wrapping-output-translations ,parameter ,@*default-output-translations*) :collect #'c))
   :test 'equal :from-end t))

(defvar *output-translations-parameter* nil)

(defun* initialize-output-translations (&optional (parameter *output-translations-parameter*))
  "read the configuration, initialize the internal configuration variable,
return the configuration"
  (setf *output-translations-parameter* parameter
        (output-translations) (compute-output-translations parameter)))

(defun* disable-output-translations ()
  "Initialize output translations in a way that maps every file to itself,
effectively disabling the output translation facility."
  (initialize-output-translations
   '(:output-translations :disable-cache :ignore-inherited-configuration)))

;; checks an initial variable to see whether the state is initialized
;; or cleared. In the former case, return current configuration; in
;; the latter, initialize.  ASDF will call this function at the start
;; of (asdf:find-system).
(defun* ensure-output-translations ()
  (if (output-translations-initialized-p)
      (output-translations)
      (initialize-output-translations)))

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

(defun* apply-output-translations (path)
  #+cormanlisp (truenamize path) #-cormanlisp
  (etypecase path
    (logical-pathname
     path)
    ((or pathname string)
     (ensure-output-translations)
     (loop :with p = (truenamize path)
       :for (source destination) :in (car *output-translations*)
       :for root = (when (or (eq source t)
                             (and (pathnamep source)
                                  (not (absolute-pathname-p source))))
                     (pathname-root p))
       :for absolute-source = (cond
                                ((eq source t) (wilden root))
                                (root (merge-pathnames* source root))
                                (t source))
       :when (or (eq source t) (pathname-match-p p absolute-source))
       :return (translate-pathname* p absolute-source destination root source)
       :finally (return p)))))

(defmethod output-files :around (operation component)
  "Translate output files, unless asked not to"
  operation component ;; hush genera, not convinced by declare ignorable(!)
  (values
   (multiple-value-bind (files fixedp) (call-next-method)
     (if fixedp
         files
         (mapcar #'apply-output-translations files)))
   t))

(defun* fasl-type (&rest keys)
  "pathname TYPE for lisp FASt Loading files"
  (declare (ignorable keys))
  #-ecl (load-time-value (pathname-type (compile-file-pathname "foo.lisp")))
  #+ecl (pathname-type (apply 'compile-file-pathname "foo.lisp" keys)))

(defun* compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
  (if (absolute-pathname-p output-file)
      ;; what cfp should be doing, w/ mp* instead of mp
      (let* ((type (pathname-type (apply 'fasl-type keys)))
             (defaults (make-pathname
                        :type type :defaults (merge-pathnames* input-file))))
        (merge-pathnames* output-file defaults))
      (apply-output-translations
       (apply 'compile-file-pathname input-file
              (if output-file keys (remove-keyword :output-file keys))))))

(defun* delete-file-if-exists (x)
  (when (and x (probe-file* x))
    (delete-file x)))

(defun* compile-file* (input-file &rest keys &key compile-check output-file &allow-other-keys)
  (let* ((keywords (remove-keyword :compile-check keys))
         (output-file (apply 'compile-file-pathname* input-file :output-file output-file keywords))
         (tmp-file (tmpize-pathname output-file))
         (status :error))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (apply 'compile-file input-file :output-file tmp-file keywords)
      (cond
        (failure-p
         (setf status *compile-file-failure-behaviour*))
        (warnings-p
         (setf status *compile-file-warnings-behaviour*))
        (t
         (setf status :success)))
      (cond
        ((and (ecase status
                ((:success :warn :ignore) t)
                ((:error nil)))
              (or (not compile-check)
                  (apply compile-check input-file :output-file tmp-file keywords)))
         (delete-file-if-exists output-file)
         (when output-truename
           (rename-file-overwriting-target output-truename output-file)
           (setf output-truename output-file)))
        (t ;; error or failed check
         (delete-file-if-exists output-truename)
         (setf output-truename nil failure-p t)))
      (values output-truename warnings-p failure-p))))

#+abcl
(defun* translate-jar-pathname (source wildcard)
  (declare (ignore wildcard))
  (flet ((normalize-device (pathname)
           (if (find :windows *features*)
               pathname
               (make-pathname :defaults pathname :device :unspecific))))
    (let* ((jar
             (pathname (first (pathname-device source))))
           (target-root-directory-namestring
             (format nil "/___jar___file___root___/~@[~A/~]"
                     (and (find :windows *features*)
                          (pathname-device jar))))
           (relative-source
             (relativize-pathname-directory source))
           (relative-jar
             (relativize-pathname-directory (ensure-directory-pathname jar)))
           (target-root-directory
             (normalize-device
              (pathname-directory-pathname
               (parse-namestring target-root-directory-namestring))))
           (target-root
             (merge-pathnames* relative-jar target-root-directory))
           (target
             (merge-pathnames* relative-source target-root)))
      (normalize-device (apply-output-translations target)))))

;;;; -----------------------------------------------------------------
;;;; Compatibility mode for ASDF-Binary-Locations

(defmethod operate :before (operation-class system &rest args &key &allow-other-keys)
  (declare (ignorable operation-class system args))
  (when (find-symbol* '#:output-files-for-system-and-operation :asdf)
    (error "ASDF 2 is not compatible with ASDF-BINARY-LOCATIONS, which you are using.
ASDF 2 now achieves the same purpose with its builtin ASDF-OUTPUT-TRANSLATIONS,
which should be easier to configure. Please stop using ASDF-BINARY-LOCATIONS,
and instead use ASDF-OUTPUT-TRANSLATIONS. See the ASDF manual for details.
In case you insist on preserving your previous A-B-L configuration, but
do not know how to achieve the same effect with A-O-T, you may use function
ASDF:ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY as documented in the manual;
call that function where you would otherwise have loaded and configured A-B-L.")))

(defun* enable-asdf-binary-locations-compatibility
    (&key
     (centralize-lisp-binaries nil)
     (default-toplevel-directory
         (subpathname (user-homedir) ".fasls/")) ;; Use ".cache/common-lisp/" instead ???
     (include-per-user-information nil)
     (map-all-source-files (or #+(or clisp ecl mkcl) t nil))
     (source-to-target-mappings nil))
  #+(or clisp ecl mkcl)
  (when (null map-all-source-files)
    (error "asdf:enable-asdf-binary-locations-compatibility doesn't support :map-all-source-files nil on CLISP, ECL and MKCL"))
  (let* ((fasl-type (fasl-type))
         (mapped-files (if map-all-source-files *wild-file*
                           (make-pathname :type fasl-type :defaults *wild-file*)))
         (destination-directory
          (if centralize-lisp-binaries
              `(,default-toplevel-directory
                ,@(when include-per-user-information
                        (cdr (pathname-directory (user-homedir))))
                :implementation ,*wild-inferiors*)
              `(:root ,*wild-inferiors* :implementation))))
    (initialize-output-translations
     `(:output-translations
       ,@source-to-target-mappings
       #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
       #+abcl (#p"/___jar___file___root___/**/*.*" (,@destination-directory))
       ((:root ,*wild-inferiors* ,mapped-files)
        (,@destination-directory ,mapped-files))
       (t t)
       :ignore-inherited-configuration))))

;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See the Manual and https://bugs.launchpad.net/asdf/+bug/485918

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
                       ;; but isn't quite in CLISP, for doesn't have :version :newest
                       (and u (equal (ignore-errors (truename u)) (truename f)) u)))
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

(defun* directory-asd-files (directory)
  (directory-files directory *wild-asd*))

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

(defun* collect-asds-in-directory (directory collect)
  (map () collect (directory-asd-files directory)))

(defun* collect-sub*directories (directory collectp recursep collector)
  (when (funcall collectp directory)
    (funcall collector directory))
  (dolist (subdir (subdirectories directory))
    (when (funcall recursep subdir)
      (collect-sub*directories subdir collectp recursep collector))))

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
    #+ecl (:tree ,(translate-logical-pathname "SYS:"))
    #+mkcl (:tree ,(translate-logical-pathname "CONTRIB:"))
    #+sbcl (:tree ,(truenamize (getenv-pathname "SBCL_HOME" :want-directory t)))
    :inherit-configuration
    #+cmu (:tree #p"modules:")
    #+scl (:tree #p"file://modules/")))
(defun* default-source-registry ()
  `(:source-registry
    #+sbcl (:directory ,(subpathname (user-homedir) ".sbcl/systems/"))
    (:directory ,(default-directory))
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
(declaim (ftype (function (t &key (:register (or symbol function))) t)
                inherit-source-registry))
(declaim (ftype (function (t &key (:register (or symbol function)) (:inherit list)) t)
                process-source-registry-directive))

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

(defun* flatten-source-registry (&optional parameter)
  (remove-duplicates
   (while-collecting (collect)
     (let ((*default-pathname-defaults* (default-directory)))
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

(defun* clear-configuration ()
  (clear-source-registry)
  (clear-output-translations))


;;; ECL and MKCL support for COMPILE-OP / LOAD-OP
;;;
;;; In ECL and MKCL, these operations produce both
;;; FASL files and the object files that they are built from.
;;; Having both of them allows us to later on reuse the object files
;;; for bundles, libraries, standalone executables, etc.
;;;
;;; This has to be in asdf.lisp and not asdf-ecl.lisp, or else it becomes
;;; a problem for asdf on ECL to compile asdf-ecl.lisp after loading asdf.lisp.
;;;
;;; Also, register-pre-built-system.

#+(or ecl mkcl)
(progn
  (defun* register-pre-built-system (name)
    (register-system (make-instance 'system :name (coerce-name name) :source-file nil)))

  #+(or (and ecl win32) (and mkcl windows))
  (unless (assoc "asd" #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* :test 'equal)
    (appendf #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* '(("asd" . si::load-source))))

  (setf #+ecl ext:*module-provider-functions* #+mkcl mk-ext::*module-provider-functions*
        (loop :for f :in #+ecl ext:*module-provider-functions*
          #+mkcl mk-ext::*module-provider-functions*
          :unless (eq f 'module-provide-asdf)
          :collect #'(lambda (name)
                       (let ((l (multiple-value-list (funcall f name))))
                         (and (first l) (register-pre-built-system (coerce-name name)))
                         (values-list l)))))

  (setf *compile-op-compile-file-function* 'compile-file-keeping-object)

  (defun* compile-file-keeping-object (input-file &rest keys &key &allow-other-keys)
    (#+ecl if #+ecl (use-ecl-byte-compiler-p) #+ecl (apply 'compile-file* input-file keys)
     #+mkcl progn
     (multiple-value-bind (object-file flags1 flags2)
         (apply 'compile-file* input-file
                #+ecl :system-p #+ecl t #+mkcl :fasl-p #+mkcl nil keys)
       (values (and object-file
                    (compiler::build-fasl
                     (compile-file-pathname object-file
                                            #+ecl :type #+ecl :fasl #+mkcl :fasl-p #+mkcl t)
                     #+ecl :lisp-files #+mkcl :lisp-object-files (list object-file))
                    object-file)
               flags1
               flags2)))))

;;;; -----------------------------------------------------------------------
;;;; Hook into REQUIRE for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL
;;;;
(defun* module-provide-asdf (name)
  (handler-bind
      ((style-warning #'muffle-warning)
       #-genera
       (missing-component (constantly nil))
       (error #'(lambda (e)
                  (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                          name e))))
    (let ((*verbose-out* (make-broadcast-stream))
          (system (find-system (string-downcase name) nil)))
      (when system
        (require-system system :verbose nil)
        t))))

#+(or abcl clisp clozure cmu ecl mkcl sbcl)
(let ((x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom))))
  (when x
    (eval `(pushnew 'module-provide-asdf
            #+abcl sys::*module-provider-functions*
            #+clisp ,x
            #+clozure ccl:*module-provider-functions*
            #+(or cmu ecl) ext:*module-provider-functions*
            #+mkcl mk-ext:*module-provider-functions*
            #+sbcl sb-ext:*module-provider-functions*))))

;;;; -------------------------------------------------------------------------
;;;; Cleanups after hot-upgrade.
;;;; Things to do in case we're upgrading from a previous version of ASDF.
;;;; See https://bugs.launchpad.net/asdf/+bug/485687
;;;;

;;; If a previous version of ASDF failed to read some configuration, try again.
(when *ignored-configuration-form*
  (clear-configuration)
  (setf *ignored-configuration-form* nil))

;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;

(defclass bundle-op (operation)
  ((build-args :initarg :args :initform nil :accessor bundle-op-build-args)
   (name-suffix :initarg :name-suffix :initform nil)
   #+ecl (type :reader bundle-op-type)
   #+ecl (lisp-files :initform nil :accessor bundle-op-lisp-files)
   #+mkcl (do-fasb :initarg :do-fasb :initform t :reader bundle-op-do-fasb-p)
   #+mkcl (do-static-library :initarg :do-static-library :initform t :reader bundle-op-do-static-library-p)))

(defclass fasl-op (bundle-op)
  ((type :initform :fasl)))

(defclass lib-op (bundle-op)
  ((type :initform :lib)))

(defclass dll-op (bundle-op)
  ((type :initform :dll)))

(defclass monolithic-bundle-op (bundle-op)
  ((prologue-code :accessor monolithic-op-prologue-code)
   (epilogue-code :accessor monolithic-op-epilogue-code)))

(defun* bundle-op-monolithic-p (op)
  (typep op 'monolithic-bundle-op))

(defclass monolithic-fasl-op (monolithic-bundle-op fasl-op) ())

(defclass monolithic-lib-op (monolithic-bundle-op lib-op)
  ((type :initform :lib)))

(defclass monolithic-dll-op (monolithic-bundle-op dll-op)
  ((type :initform :dll)))

(defclass program-op (monolithic-bundle-op)
  ((type :initform :program)))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                       &key (name-suffix nil name-suffix-p)
                                       &allow-other-keys)
  (declare (ignorable initargs name-suffix))
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
          (if (bundle-op-monolithic-p instance) ".system-and-dependencies" ".system")))
  (when (typep instance 'monolithic-bundle-op)
    (destructuring-bind (&rest original-initargs
                         &key lisp-files prologue-code epilogue-code
                         &allow-other-keys)
        (slot-value instance 'original-initargs)
      (setf (slot-value instance 'original-initargs)
            (remove-keys '(lisp-files epilogue-code prologue-code) original-initargs)
            (monolithic-op-prologue-code instance) prologue-code
            (monolithic-op-epilogue-code instance) epilogue-code)
      #-ecl (assert (null lisp-files))
      #+ecl (setf (bundle-op-lisp-files instance) lisp-files)))
  (setf (bundle-op-build-args instance)
        (remove-keys '(type monolithic name-suffix)
                     (slot-value instance 'original-initargs))))

(defmethod bundle-op-build-args :around ((o lib-op))
  (declare (ignorable o))
  (let ((args (call-next-method)))
    (remf args :ld-flags)
    args))

(defun* gather-components (operation system &key other-systems filter-type include-self)
  ;; This function creates a list of actions pairing the operation with sub-components of system
  ;; and its dependencies if requested.
  ;; This list may be restricted to sub-components of SYSTEM
  ;; if GATHER-ALL = NIL (default), and it may include the system itself.
  (let ((tree (traverse-sequentially (make-operation 'load-op) system
                                     :force (if other-systems :all t)
                                     :force-not (if other-systems nil :all))))
    `(,@(loop :for (op . component) :in tree
              :when (and (typep op 'load-op)
                         (typep component filter-type))
                :collect (progn
                           (when (eq component system) (setf include-self nil))
                           `(,operation . ,component)))
      ,@(and include-self `((,operation . ,system))))))

(defgeneric* trivial-system-p (component))

(defun* user-system-p (s)
  (and (typep s 'system)
       (not (builtin-system-p s))
       (not (trivial-system-p s))))

(deftype user-system () '(and system (satisfies user-system-p)))

;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component)
;;; which contains all the dependencies of this bundle.
;;; This list is used by TRAVERSE and also by INPUT-FILES.
;;; The dependencies depend on the strategy, as explained below.
;;;
(defgeneric* bundle-sub-operations (operation component))
;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
;;; Gather the static libraries of all components.
;;;
(defmethod bundle-sub-operations ((o monolithic-bundle-op) c)
  (gather-components (find-operation o 'lib-op) c :filter-type 'user-system :include-self t))

;;;
;;; STATIC LIBRARIES
;;;
;;; Gather the object files of all components
;;; and, if monolithic, also of systems and subsystems.
;;;
(defmethod bundle-sub-operations ((o lib-op) c)
  (gather-components (find-operation o 'compile-op) c
                     :other-systems (bundle-op-monolithic-p o)
                     :filter-type '(not system)))
;;;
;;; SHARED LIBRARIES
;;;
;;; Gather the dynamically linked libraries of all components.
;;; They will be linked into this new shared library,
;;; together with the static library of this module.
;;;
(defmethod bundle-sub-operations ((o dll-op) c)
  `((,(find-operation o 'lib-op) . ,c)))
;;;
;;; FASL FILES
;;;
;;; Gather the statically linked library of this component.
;;;
(defmethod bundle-sub-operations ((o fasl-op) c)
  `((,(find-operation o 'lib-op) . ,c)))

#-mkcl
(defmethod component-depends-on ((o bundle-op) (c system))
  `(,@(loop :for (op . dep) :in (bundle-sub-operations o c)
            :when (user-system-p dep) :collect (list op dep))
    ,@(call-next-method)))

(defmethod component-depends-on ((o lib-op) (c system))
  (declare (ignorable o))
  `((compile-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((o bundle-op) c)
  (declare (ignorable o c))
  nil)

#-mkcl
(defmethod input-files ((o bundle-op) (c system))
  (loop :for (sub-op . sub-c) :in (bundle-sub-operations o c)
        :nconc (output-files sub-op sub-c)))

#-mkcl
(defmethod output-files ((o bundle-op) (c system))
  (list (compile-file-pathname
         (make-pathname
          :name (strcat (component-name c) (slot-value o 'name-suffix)
                        #|"-" (string-downcase (implementation-type))|#)
          :type "lisp"
          :defaults (system-source-directory c))
         #+ecl :type #+ecl (bundle-op-type o))))

(defun* select-operation (monolithic type)
  (ecase type
    ((:binary)
     (if monolithic 'monolithic-binary-op 'binary-op))
    ((:dll :shared-library)
     (if monolithic 'monolithic-dll-op 'dll-op))
    ((:lib :static-library)
     (if monolithic 'monolithic-lib-op 'lib-op))
    ((:fasl)
     (if monolithic 'monolithic-fasl-op 'fasl-op))
    ((:program)
     'program-op)))

(defun* make-build (system &rest args &key (monolithic nil) (type :fasl)
                   (move-here nil move-here-p)
                   &allow-other-keys)
  (let* ((operation-name (select-operation monolithic type))
         (move-here-path (if (and move-here
                                  (typep move-here '(or pathname string)))
                             (pathname move-here)
                             (merge-pathnames "./asdf-output/")))
         (operation (apply #'operate operation-name
                           system
                           (remove-keys '(monolithic type move-here) args)))
         (system (find-system system))
         (files (and system (output-files operation system))))
    (if (or move-here (and (null move-here-p)
                           (member operation-name '(:program :binary))))
        (loop :with dest-path = (truename (ensure-directories-exist move-here-path))
              :for f :in files
              :for new-f = (make-pathname :name (pathname-name f)
                                :type (pathname-type f)
                                :defaults dest-path)
              :do (rename-file-overwriting-target f new-f)
              :collect new-f)
        files)))

;;;
;;; LOAD-FASL-OP
;;;
;;; This is like ASDF's LOAD-OP, but using monolithic fasl files.
;;;

(defclass load-fasl-op (basic-load-op) ())

(defmethod component-depends-on ((o load-fasl-op) (c system))
  (declare (ignorable o))
  `((load-fasl-op ,@(loop :for dep :in (component-sibling-dependencies c)
                     :collect (resolve-dependency-spec c dep)))
    (,(if (user-system-p c) 'fasl-op 'load-op) ,c)
    ,@(call-next-method)))

(defmethod input-files ((o load-fasl-op) (c system))
  (when (user-system-p c)
    (output-files (find-operation o 'fasl-op) c)))

(defmethod perform ((o load-fasl-op) c)
  (declare (ignorable o c))
  nil)

(defmethod perform ((o load-fasl-op) (c system))
  (aif (first (input-files o c)) (load it)))

(defmethod mark-operation-done :after ((o load-fasl-op) (c system))
  (mark-operation-done (find-operation o 'load-op) c)) ; need we recurse on gather-components?

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF systems in precompiled form.
;;; Only useful when the dependencies have also been precompiled.
;;;

(defclass compiled-file (file-component)
  ((type :initform #-(or ecl mkcl) (fasl-type) #+(or ecl mkcl) "fasb")))

(defmethod trivial-system-p ((s system))
  (every #'(lambda (c) (typep c 'compiled-file)) (component-children s)))

(defmethod output-files (o (c compiled-file))
  (declare (ignorable o c))
  nil)
(defmethod input-files (o (c compiled-file))
  (declare (ignorable o))
  (load (component-pathname c)))
(defmethod perform ((o load-op) (c compiled-file))
  (declare (ignorable o))
  (load (component-pathname c)))
(defmethod perform ((o load-source-op) (c compiled-file))
  (perform (find-operation o 'load-op) c))
(defmethod perform ((o load-fasl-op) (c compiled-file))
  (perform (find-operation o 'load-op) c))
(defmethod perform (o (c compiled-file))
  (declare (ignorable o c))
  nil)

;;;
;;; Pre-built systems
;;;
(defclass prebuilt-system (system)
  ((static-library :accessor prebuilt-system-static-library :initarg :lib)))

(defmethod builtin-system-p ((s system))
  (let* ((system (find-system s nil))
         (sysdir (and system (component-pathname system)))
         (impdir (lisp-implementation-directory :truename t)))
    (and sysdir impdir (pathname-match-p (truename sysdir) (wilden impdir)) t)))

(defmethod trivial-system-p ((s prebuilt-system))
  (declare (ignorable s))
  t)

(defmethod output-files ((o lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (values (list (prebuilt-system-static-library c))
          t)) ; Advertise that we do not want this path renamed by asdf-output-translations

(defmethod perform ((o lib-op) (c prebuilt-system))
  (first (output-files o c)))

(defmethod component-depends-on ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o monolithic-lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (error "Prebuilt system ~S shipped with ECL can not be used in a monolithic library operation." c))

(defmethod bundle-sub-operations ((o monolithic-bundle-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

;;;
;;; PREBUILT SYSTEM CREATOR
;;;

(defclass binary-op (bundle-op)
  ())

(defclass monolithic-binary-op (binary-op monolithic-bundle-op)
  ())

(defun* binary-op-dependencies (o s)
  (multiple-value-bind (lib-op fasl-op)
      (if (bundle-op-monolithic-p o)
          (values 'monolithic-lib-op 'monolithic-fasl-op)
          (values 'lib-op 'fasl-op))
    `((,(find-operation o lib-op) ,s)
      (,(find-operation o fasl-op) ,s))))

(defmethod component-depends-on ((o binary-op) (s system))
  `(,@(loop :for dep :in (binary-op-dependencies o s)
            :append (apply #'component-depends-on dep))
    ,@(call-next-method)))

(defmethod input-files ((o binary-op) (s system))
  (loop :for dep :in (binary-op-dependencies o s)
        :append (apply #'input-files dep)))

(defmethod output-files ((o binary-op) (s system))
  (list* (merge-pathnames* (make-pathname :name (component-name s)
                                          :type "asd")
                           (component-relative-pathname s))
         (loop :for dep :in (binary-op-dependencies o s)
               :append (apply #'output-files dep))))

(defmethod perform ((o binary-op) (s system))
  (let* ((dependencies (binary-op-dependencies o s))
         (library (first (apply #'output-files (first dependencies))))
         (fasl (first (apply #'output-files (second dependencies))))
         (filename (first (output-files o s)))
         (name (component-name s))
         (name-keyword (intern (string name) (find-package :keyword))))
    (dolist (dep dependencies)
      (apply #'perform dep))
    (with-open-file (s filename :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (format s ";;; Prebuilt ASDF definition for system ~A" name)
      (format s ";;; Built for ~A ~A on a ~A/~A ~A"
              (lisp-implementation-type)
              (lisp-implementation-version)
              (software-type)
              (machine-type)
              (software-version))
      (let ((*package* (find-package :keyword)))
        (pprint `(defsystem ,name-keyword
                     :class asdf::prebuilt-system
                     :components ((:compiled-file ,(pathname-name fasl)))
                     :lib ,(make-pathname :name (pathname-name library)
                                          :type (pathname-type library)))
                s)))))

(defun* copy-stream-to-stream (input output &key (element-type 'character) (buffer-size 8192))
  "Copy the contents of the INPUT stream into the OUTPUT stream,
using WRITE-SEQUENCE and a sensibly sized buffer." ; copied from xcvb-driver
  (with-open-stream (input input)
    (loop
      :for buffer = (make-array (list buffer-size) :element-type element-type)
      :for end = (read-sequence buffer input)
      :until (zerop end)
      :do (write-sequence buffer output :end end)
          (when (< end buffer-size) (return)))))

(defun* concatenate-files (inputs output)
  (with-open-file (o output :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :rename-and-delete)
    (dolist (input inputs)
      (with-open-file (i input :element-type '(unsigned-byte 8)
                               :direction :input :if-does-not-exist :error)
        (copy-stream-to-stream i o :element-type '(unsigned-byte 8))))))

(defun* combine-fasls (inputs output)
  #-(or allegro clisp clozure cmu lispworks sbcl scl xcl)
  (declare (ignore inputs output))
  #-(or allegro clisp clozure cmu lispworks sbcl scl xcl)
  (error "~S is not supported on ~A" 'combine-fasls (implementation-type))
  #+clozure (ccl:fasl-concatenate output inputs :if-exists :supersede)
  #+(or allegro clisp cmu sbcl scl xcl) (concatenate-files inputs output)
  #+lispworks
  (let (fasls)
    (unwind-protect
         (progn
           (loop :for i :in inputs
                 :for n :from 1
                 :for f = (add-pathname-suffix
                           output (format nil "-FASL~D" n))
                 :do #-lispworks-personal-edition (lispworks:copy-file i f)
                     #+lispworks-personal-edition (concatenate-files (list i) f)
                     (push f fasls))
           (ignore-errors (lispworks:delete-system :fasls-to-concatenate))
           (eval `(scm:defsystem :fasls-to-concatenate
                    (:default-pathname ,(pathname-directory-pathname output))
                    :members
                    ,(loop :for f :in (reverse fasls)
                           :collect `(,(namestring f) :load-only t))))
           (scm:concatenate-system output :fasls-to-concatenate))
      (loop :for f :in fasls :do (ignore-errors (delete-file f)))
      (ignore-errors (lispworks:delete-system :fasls-to-concatenate)))))

#-(or ecl mkcl)
(defmethod perform ((o bundle-op) (c system))
  (let* ((input-files (input-files o c))
         (fasl-files (remove (fasl-type) input-files :key #'pathname-type :test-not #'string=))
         (non-fasl-files (remove (fasl-type) input-files :key #'pathname-type :test #'string=))
         (output-files (output-files o c))
         (output-file (first output-files)))
    (when input-files
      (assert output-files)
      (when non-fasl-files
        (error "On ~A, asdf-bundle can only bundle FASL files, but these were also produced: ~S"
               (implementation-type) non-fasl-files))
      (when (and (typep o 'monolithic-bundle-op)
                 (or (monolithic-op-prologue-code o) (monolithic-op-epilogue-code o)))
        (error "prologue-code and epilogue-code are not supported on ~A"
               (implementation-type)))
      (with-staging-pathname (output-file)
        (combine-fasls fasl-files output-file)))))

(defmethod output-files ((o fasl-op) (c source-file))
  (declare (ignorable o c))
  nil)

(defmethod input-files ((o fasl-op) (c source-file))
  (declare (ignorable o c))
  nil)

(defclass precompiled-system (system)
  ((fasl :initarg :fasl :reader %system-fasl)))

(defgeneric* system-fasl (system))
(defmethod system-fasl ((system precompiled-system))
  (let* ((f (%system-fasl system))
         (p (etypecase f
              ((or pathname string) f)
              (function (funcall f))
              (cons (eval f)))))
    (pathname p)))

(defmethod input-files ((o load-op) (s precompiled-system))
  (declare (ignorable o))
  (list (system-fasl s)))

(defmethod component-depends-on ((o load-fasl-op) (s precompiled-system))
  (declare (ignorable o))
  `((load-op ,s) ,@(call-next-method)))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+ecl
(defmethod output-files ((o fasl-op) (c system))
  (declare (ignorable o c))
  (loop :for file :in (call-next-method)
        :collect (make-pathname :type "fasb" :defaults file)))

#+ecl
(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (remove "fas" (input-files o c)
                               :key #'pathname-type :test #'string=))
         (output (output-files o c)))
    (apply #'c::builder (bundle-op-type o) (first output)
           :lisp-files (append object-files (bundle-op-lisp-files o))
           (append (bundle-op-build-args o)
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-prologue-code o))
                     `(:prologue-code ,(monolithic-op-prologue-code o)))
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-epilogue-code o))
                     `(:epilogue-code ,(monolithic-op-epilogue-code o)))))))

#+mkcl
(progn
;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component) which contains all the
;;; dependencies of this bundle.
;;;

(defun* mkcl-bundle-sub-operations (op sys)
  (gather-components (find-operation op 'compile-op) sys
                     :other-systems nil
                     :filter-type '(not system)))

(defun* files-to-bundle (operation system)
  (loop :for (o . c) :in (mkcl-bundle-sub-operations operation system)
    :for sub-files = (output-files o c)
    :when sub-files :collect (first sub-files)))

(defmethod component-depends-on ((o bundle-op) (c system))
  (declare (ignorable o))
  `((compile-op ,c) ,@(call-next-method)))

(defmethod output-files ((o bundle-op) (c system))
  (let* ((name (component-name c))
         (static-lib-name (merge-pathnames
                           (compiler::builder-internal-pathname name :static-library)
                           (component-relative-pathname c)))
         (fasl-bundle-name (merge-pathnames
                            (compiler::builder-internal-pathname name :fasb)
                            (component-relative-pathname c))))
    (list static-lib-name fasl-bundle-name)))

(defmethod perform ((o bundle-op) (c system))
  (let ((object-files (files-to-bundle o c))
        (output (output-files o c)))
    (when (bundle-op-do-static-library-p o)
      (apply #'compiler::build-static-library (first output)
             :lisp-object-files object-files (bundle-op-build-args o)))
    (when (bundle-op-do-fasb-p o)
      (apply #'compiler::build-bundle (second output)
             :lisp-object-files object-files (bundle-op-build-args o)))))

(defun* bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
  (declare (ignore force verbose version))
  (apply #'operate 'bundle-op system args))
);mkcl

;;;; -----------------------------------------------------------------
;;;; Done!
(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))

#+allegro
(eval-when (:compile-toplevel :execute)
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* *acl-warn-save*)))

#+(or ecl mkcl)
(pushnew '("fasb" . si::load-binary) si:*load-hooks* :test 'equal :key 'car)

(dolist (f '(:asdf :asdf2 :asdf2.27)) (pushnew f *features*))

(provide :asdf)

;;; Local Variables:
;;; mode: lisp
;;; End:
