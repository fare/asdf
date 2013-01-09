;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(asdf/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:fmakunbound #:component-load-dependencies)
  (:use :common-lisp :asdf/implementation :asdf/utility :asdf/pathname :asdf/os
   :asdf/component :asdf/system :asdf/operation :asdf/action
   :asdf/lisp-build :asdf/operate :asdf/output-translations)
  (:export
   #:*asdf-verbose*
   #:component-load-dependencies
   #:enable-asdf-binary-locations-compatibility
   #:merge-component-name-type
   #:operation-forced
   #:operation-on-failure
   #:operation-on-warnings
   #:run-shell-command
   #:system-definition-pathname))
(in-package :asdf/backward-interface)

(defvar *asdf-verbose* nil) ; worked around by cl-protobufs. It was a mistake to introduce it. mea culpa -fare

(defun* component-load-dependencies (component)
  ;; Old deprecated name for the same thing. Please update your software.
  (component-sibling-dependencies component))

(defun* merge-component-name-type (name &key type defaults)
  ;; For backward-compatibility only, for people using internals.
  ;; Will be removed in a future release, e.g. 2.016.
  (warn "Please don't use ASDF::MERGE-COMPONENT-NAME-TYPE. Use ASDF:COERCE-PATHNAME.")
  (coerce-pathname name :type type :defaults defaults))

(defgeneric* operation-forced (operation)) ;; Used by swank.asd for swank-loader.
(defmethod operation-forced ((o operation)) (getf (operation-original-initargs o) :force))

(defgeneric* operation-on-warnings (operation))
(defgeneric* operation-on-failure (operation))
#-gcl<2.7 (defgeneric* (setf operation-on-warnings) (x operation))
#-gcl<2.7 (defgeneric* (setf operation-on-failure) (x operation))
(defmethod operation-on-warnings ((o operation))
  (declare (ignorable o)) *compile-file-warnings-behaviour*)
(defmethod operation-on-failure ((o operation))
  (declare (ignorable o)) *compile-file-failure-behaviour*)
(defmethod (setf operation-on-warnings) (x (o operation))
  (declare (ignorable o)) (setf *compile-file-warnings-behaviour* x))
(defmethod (setf operation-on-failure) (x (o operation))
  (declare (ignorable o)) (setf *compile-file-failure-behaviour* x))

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


;;;; ASDF-Binary-Locations compatibility

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

(defmethod operate :before (operation-class system &rest args &key &allow-other-keys)
  (declare (ignorable operation-class system args))
  (when (find-symbol* '#:output-files-for-system-and-operation :asdf nil)
    (error "ASDF 2 is not compatible with ASDF-BINARY-LOCATIONS, which you are using.
ASDF 2 now achieves the same purpose with its builtin ASDF-OUTPUT-TRANSLATIONS,
which should be easier to configure. Please stop using ASDF-BINARY-LOCATIONS,
and instead use ASDF-OUTPUT-TRANSLATIONS. See the ASDF manual for details.
In case you insist on preserving your previous A-B-L configuration, but
do not know how to achieve the same effect with A-O-T, you may use function
ASDF:ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY as documented in the manual;
call that function where you would otherwise have loaded and configured A-B-L.")))


;;;; run-shell-command
;;
;; run-shell-command functions for other lisp implementations will be
;; gratefully accepted, if they do the same thing.
;; If the docstring is ambiguous, send a bug report.
;;
;; WARNING! The function below is mostly dysfunctional.
;; For instance, it will probably run fine on most implementations on Unix,
;; which will hopefully use the shell /bin/sh (which we force in some cases)
;; which is hopefully reasonably compatible with a POSIX *or* Bourne shell.
;; But behavior on Windows may vary wildly between implementations,
;; either relying on your having installed a POSIX sh, or going through
;; the CMD.EXE interpreter, for a totally different meaning, depending on
;; what is easily expressible in said implementation.
;;
;; We probably should move this functionality to its own system and deprecate
;; use of it from the asdf package. However, this would break unspecified
;; existing software, so until a clear alternative exists, we can't deprecate
;; it, and even after it's been deprecated, we will support it for a few
;; years so everyone has time to migrate away from it. -- fare 2009-12-01
;;
;; As a suggested replacement which is portable to all ASDF-supported
;; implementations and operating systems except Genera, I recommend
;; xcvb-driver's xcvb-driver:run-program/ and its derivatives.

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

