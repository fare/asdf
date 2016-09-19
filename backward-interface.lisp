;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(uiop/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/operation :asdf/action
   :asdf/lisp-action :asdf/plan :asdf/operate :asdf/output-translations)
  (:export
   #:*asdf-verbose*
   #:operation-error #:compile-error #:compile-failed #:compile-warned
   #:error-component #:error-operation #:traverse
   #:component-load-dependencies
   #:enable-asdf-binary-locations-compatibility
   #:operation-forced
   #:operation-on-failure #:operation-on-warnings #:on-failure #:on-warnings
   #:component-property
   #:run-shell-command
   #:system-definition-pathname
   #:explain))
(in-package :asdf/backward-interface)

(with-upgradability ()
  ;; These conditions from ASDF 1 and 2 are used by many packages in Quicklisp;
  ;; but ASDF3 replaced them with somewhat different variants of uiop:compile-condition
  ;; that do not involve ASDF actions.
  ;; TODO: find the offenders and stop them.
  (define-condition operation-error (error) ;; Bad, backward-compatible name
    ;; Used by SBCL, cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
    ((component :reader error-component :initarg :component)
     (operation :reader error-operation :initarg :operation))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                       (type-of c) (error-operation c) (error-component c)))))
  (define-condition compile-error (operation-error) ())
  (define-condition compile-failed (compile-error) ())
  (define-condition compile-warned (compile-error) ())

  (defun component-load-dependencies (component)
    "DEPRECATED. Please use COMPONENT-SIDEWAY-DEPENDENCIES instead."
    ;; Old deprecated name for the same thing. Please update your software.
    (component-sideway-dependencies component))

  (defgeneric operation-forced (operation)
    (:documentation "DEPRECATED. Assume it's (constantly t) instead."))
  ;; This method exists for backward compatibility with swank.asd, its only user,
  ;; that still uses it as of 2016-09-21.
  ;;
  ;; The magic PERFORM method in swank.asd only actually loads swank if it sees that
  ;; the operation was forced. But except for the first time, the only reason the action
  ;; would be performed to begin with is because it was forced; and the first time over,
  ;; it doesn't hurt that :reload t :delete t should be used. So the check is redundant.
  ;; More generally, if you have to do something when the operation was forced,
  ;; you should also do it when not, and vice-versa, because it really shouldn't matter.
  ;; Thus, the backward-compatible thing to do is to always return T.
  (defmethod operation-forced ((o operation)) t)


  ;; These old interfaces from ASDF1 have never been very meaningful
  ;; but are still used in obscure places.
  (defgeneric operation-on-warnings (operation)
    (:documentation "DEPRECATED. Please use UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead."))
  (defgeneric operation-on-failure (operation)
    (:documentation "DEPRECATED. Please use UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead."))
  (defgeneric (setf operation-on-warnings) (x operation)
    (:documentation "DEPRECATED. Please SETF UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead."))
  (defgeneric (setf operation-on-failure) (x operation)
    (:documentation "DEPRECATED. Please SETF UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead."))
  (defmethod operation-on-warnings ((o operation))
    *compile-file-warnings-behaviour*)
  (defmethod operation-on-failure ((o operation))
    *compile-file-failure-behaviour*)
  (defmethod (setf operation-on-warnings) (x (o operation))
    (setf *compile-file-warnings-behaviour* x))
  (defmethod (setf operation-on-failure) (x (o operation))
    (setf *compile-file-failure-behaviour* x))

  (defun system-definition-pathname (x)
    ;; As of 2.014.8, we mean to make this function obsolete,
    ;; but that won't happen until all clients have been updated.
    ;;(cerror "Use ASDF:SYSTEM-SOURCE-FILE instead"
    "DEPRECATED. This function used to expose ASDF internals with subtle
differences with respect to user expectations, that have been refactored
away since. We recommend you use ASDF:SYSTEM-SOURCE-FILE instead for a
mostly compatible replacement that we're supporting, or even
ASDF:SYSTEM-SOURCE-DIRECTORY or ASDF:SYSTEM-RELATIVE-PATHNAME
if that's whay you mean." ;;)
    (system-source-file x))


  ;; TRAVERSE is the function used to compute a plan in ASDF 1 and 2.
  ;; It was never officially exposed but some people still used it.
  (defgeneric* (traverse) (operation component &key &allow-other-keys)
    (:documentation
     "Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))
  (define-convenience-action-methods traverse (operation component &key))

  (defmethod traverse ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
    (plan-actions (apply 'make-plan plan-class o c keys))))


;;;; ASDF-Binary-Locations compatibility
;; This remains supported for legacy user, but not recommended for new users.
;; We suspect there are no more legacy users in 2016.
(with-upgradability ()
  (defun enable-asdf-binary-locations-compatibility
      (&key
       (centralize-lisp-binaries nil)
       (default-toplevel-directory
        (subpathname (user-homedir-pathname) ".fasls/")) ;; Use ".cache/common-lisp/" instead ???
       (include-per-user-information nil)
       (map-all-source-files (or #+(or clasp clisp ecl mkcl) t nil))
       (source-to-target-mappings nil)
       (file-types `(,(compile-file-type)
                     "build-report"
                     #+clasp (compile-file-type :output-type :object)
                     #+ecl (compile-file-type :type :object)
                     #+mkcl (compile-file-type :fasl-p nil)
                     #+clisp "lib" #+sbcl "cfasl"
                     #+sbcl "sbcl-warnings" #+clozure "ccl-warnings")))
    #+(or clasp clisp ecl mkcl)
    (when (null map-all-source-files)
      (error "asdf:enable-asdf-binary-locations-compatibility doesn't support :map-all-source-files nil on CLISP, ECL and MKCL"))
    (let* ((patterns (if map-all-source-files (list *wild-file*)
                         (loop :for type :in file-types
                               :collect (make-pathname :type type :defaults *wild-file*))))
           (destination-directory
             (if centralize-lisp-binaries
                 `(,default-toplevel-directory
                   ,@(when include-per-user-information
                       (cdr (pathname-directory (user-homedir-pathname))))
                   :implementation ,*wild-inferiors*)
                 `(:root ,*wild-inferiors* :implementation))))
      (initialize-output-translations
       `(:output-translations
         ,@source-to-target-mappings
         #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
         #+abcl (#p"/___jar___file___root___/**/*.*" (,@destination-directory))
         ,@(loop :for pattern :in patterns
                 :collect `((:root ,*wild-inferiors* ,pattern)
                            (,@destination-directory ,pattern)))
         (t t)
         :ignore-inherited-configuration))))

  (defmethod operate :before (operation-class system &rest args &key &allow-other-keys)
    (declare (ignore operation-class system args))
    (when (find-symbol* '#:output-files-for-system-and-operation :asdf nil)
      (error "ASDF 2 is not compatible with ASDF-BINARY-LOCATIONS, which you are using.
ASDF 2 now achieves the same purpose with its builtin ASDF-OUTPUT-TRANSLATIONS,
which should be easier to configure. Please stop using ASDF-BINARY-LOCATIONS,
and instead use ASDF-OUTPUT-TRANSLATIONS. See the ASDF manual for details.
In case you insist on preserving your previous A-B-L configuration, but
do not know how to achieve the same effect with A-O-T, you may use function
ASDF:ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY as documented in the manual;
call that function where you would otherwise have loaded and configured A-B-L."))))


;;; run-shell-command
;; WARNING! The function below is not just deprecated but also dysfunctional.
;; Please use asdf/run-program:run-program instead.
(with-upgradability ()
  (defun run-shell-command (control-string &rest args)
    "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code.

PLEASE DO NOT USE.
Deprecated function, for backward-compatibility only.
Please use UIOP:RUN-PROGRAM instead."
    (let ((command (apply 'format nil control-string args)))
      (asdf-message "; $ ~A~%" command)
      (let ((exit-code
              (ignore-errors
               (nth-value 2 (run-program command :force-shell t :ignore-error-status t
                                                 :output *verbose-out*)))))
        (typecase exit-code
          ((integer 0 255) exit-code)
          (t 255))))))


(with-upgradability ()
  (defvar *asdf-verbose* nil)) ;; backward-compatibility with ASDF2 only. Unused.


;;; backward-compatibility methods. Do NOT use in new code. NOT SUPPORTED.
(with-upgradability ()
  (defgeneric component-property (component property))
  (defgeneric (setf component-property) (new-value component property))

  (defmethod component-property ((c component) property)
    (cdr (assoc property (slot-value c 'properties) :test #'equal)))

  (defmethod (setf component-property) (new-value (c component) property)
    (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
      (if a
          (setf (cdr a) new-value)
          (setf (slot-value c 'properties)
                (acons property new-value (slot-value c 'properties)))))
    new-value))


;;; This method survives from ASDF 1, but really it is superseded by action-description.
(with-upgradability ()
  (defgeneric* (explain) (operation component)
    (:documentation "Display a message describing an action.
DEPRECATED. Use ASDF:ACTION-DESCRIPTION and/or ASDF::FORMAT-ACTION instead."))
  (defmethod explain ((o operation) (c component))
    (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (action-description o c)))
  (define-convenience-action-methods explain (operation component)))


