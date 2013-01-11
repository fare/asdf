;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(asdf/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:use :common-lisp :asdf/utility :asdf/pathname :asdf/os :asdf/run-program
   :asdf/upgrade :asdf/component :asdf/system :asdf/operation :asdf/action
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
;; WARNING! The function below is dysfunctional and deprecated.
;; Please use asdf/run-program:run-program/ instead.

(defun* run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (let ((command (apply 'format nil control-string args)))
    (asdf-message "; $ ~A~%" command)
    (run-program/ command :force-shell t :output *verbose-out*)))
