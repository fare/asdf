":" ; exec cl-launch "$0" "$@" # -*- Lisp -*-
#|
Usage: ./tools/asdf-tools install-asdf-as-module l=lispworks

This script will install the current version of ASDF
as a module pre-compiled for your implementation,
as specified by option -l (--lisp) of cl-launch,
so you can (require "asdf") within your implementation
and have it load a recent ASDF instead of an outdated one.

This file requires cl-launch 4 and works on most implementations.
It notably doesn't work on:
* ABCL, that keeps ASDF in its jar, but that's OK because
 ABCL has a recent enough ASDF3 that is capable of upgrading itself.
* On MKCL and ECL, more work is needed to take into account
 the linkable variant of ASDF, that may be a .o or a .lib.
 Also, MKCL now delivers UIOP separately from ASDF, which is great,
 but requires support. Happily, both ECL and MKCL tend to sport
 a recent ASDF 3, too.
* GCL, that doesn't have a usable REQUIRE mechanism.
* mocl, that doesn't support ASDF 3 yet.
* Corman Lisp, RMCL, Genera, that are obsolete anyway.

|#
;;; Ensure we load and configure this particular ASDF
#-cl-launch
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :cl-launch *features*) ;; (not necessary if we're invoked via cl-launch)
    (load (make-pathname
           :name "load-asdf" :type "lisp" :defaults
           (or *compile-file-truename* *load-truename* (truename *default-pathname-defaults*))))))

(in-package :asdf)

(defvar *asdf-dir*
  (ensure-pathname (system-relative-pathname :asdf ())
                   :want-physical t :want-absolute t
                   :want-existing t :truename t))

(defun asdf-module-directory ()
  #+allegro #p"sys:code;"
  #+clisp (subpathname custom:*lib-directory* "asdf/")
  #+clozure #p"ccl:tools;"
  #+cmu #p"modules:asdf/"
  #+(or ecl mkcl) #p"sys:"
  #+gcl system:*system-directory*
  #+lispworks (system:lispworks-dir "load-on-demand/utilities/")
  #+sbcl (subpathname (sb-int:sbcl-homedir-pathname) "contrib/")
  #+scl #p"file://modules/"
  #+xcl ext:*xcl-home*
  #-(or allegro clisp clozure cmu ecl gcl lispworks mkcl sbcl scl xcl)
  (error "asdf-module-directory not implemented on ~A" (implementation-type)))

(defun asdf-module-fasl ()
  #+allegro
  (flet ((pathname-key (x)
           (let ((type (pathname-type x)))
             (cond
               ((and (stringp type) (every #'digit-char-p type)) (parse-integer type))
               ((equal type "fasl") 0)
               (t -1)))))
    (first (sort (directory (merge-pathnames* "asdf.*" (asdf-module-directory)))
                 #'> :key #'pathname-key)))
  #+(or clisp clozure cmu ecl gcl lispworks mkcl sbcl scl xcl)
  (compile-file-pathname (subpathname (truename (asdf-module-directory)) "asdf.lisp"))
  ;; ECL and MKCL not really supported at this point. See above.
  #-(or allegro clisp clozure cmu gcl lispworks sbcl scl xcl)
  (error "Not implemented on ~A" (implementation-type)))

(defun install-asdf-as-module ()
  (let* ((fasl (asdf-module-fasl))
         (orig (add-pathname-suffix fasl "-orig")))
    (ensure-directories-exist (translate-logical-pathname fasl))
    (when (and (probe-file* fasl) (not (probe-file* orig)))
      (rename-file-overwriting-target fasl orig))
    (compile-file* (subpathname *asdf-dir* "build/asdf.lisp") :output-file fasl)))

(uiop:writeln (multiple-value-list (install-asdf-as-module)))
