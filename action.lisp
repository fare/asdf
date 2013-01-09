;;;; -------------------------------------------------------------------------
;;;; Actions

(asdf/package:define-package :asdf/action
  (:recycle :asdf/action :asdf)
  (:use :common-lisp :asdf/implementation :asdf/utility :asdf/pathname :asdf/os
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation)
  #+gcl<2.7 (:shadowing-import-from :asdf/implementation #:type-of)
  (:fmakunbound
   #:explain #:output-files #:perform #:perform-with-restarts
   #:operation-done-p #:compute-action-stamp #:component-depends-on #:mark-operation-done)
  (:export
   #:action
   #:explain #:operation-description
   #:downward-operation #:upward-operation
   #:file-component
   #:source-file #:c-source-file #:java-source-file
   #:static-file #:doc-file #:html-file
   #:operation-error #:error-component #:error-operation
   #:component-depends-on #:component-self-dependencies
   #:input-files #:output-files #:output-file #:operation-done-p
   #:action-status #:action-stamp #:action-done-p
   #:component-operation-time #:mark-operation-done #:compute-action-stamp
   #:perform #:perform-with-restarts #:retry #:accept))
(in-package :asdf/action)

(deftype action () '(cons operation component)) ;; a step to be performed while building the system

;;;; self-description

(defgeneric* operation-description (operation component) ;; ASDF3: rename to action-description
  (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))
(defmethod operation-description (operation component)
  (format nil (compatfmt "~@<~A on ~A~@:>")
          (class-of operation) component))

(defgeneric* explain (operation component))
(defmethod explain ((o operation) (c component))
  (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (operation-description o c)))


;;;; Error

(define-condition operation-error (error) ;; Bad, backward-compatible name
  ;; We want to rename it to action-error, but that breaks upgrade on SBCL.
  ;; Before to rename it, fix these other culprits, too:
  ;; cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
               (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                       (type-of c) (error-operation c) (error-component c)))))


;;;; Dependencies

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

(defmethod component-depends-on ((o operation) (c component))
  (cdr (assoc (type-of o) (component-in-order-to c)))) ; User-specified in-order dependencies

(defmethod component-self-dependencies ((o operation) (c component))
  (loop :for (o-spec . c-spec) :in (component-depends-on o c)
        :unless (eq o-spec 'feature) ;; avoid the FEATURE "feature"
          :when (find c c-spec :key #'(lambda (dep) (resolve-dependency-spec c dep)))
            :collect (cons (find-operation o o-spec) c)))

;;;; upward-operation, downward-operation
;; These together handle actions that propagate along the component hierarchy.
;; Downward operations like load-op or compile-op propagate down the hierarchy:
;; operation on a parent depends-on operation on its children.
(defclass downward-operation (operation) ())
;; Upward operations like prepare-op propagate up the component hierarchy:
;; operation on a child depends-on operation on its parent.
(defclass upward-operation (operation) ())

(defmethod component-depends-on ((o downward-operation) (c parent-component))
  `((,o ,@(component-children c)) ,@(call-next-method)))
;; For backward-compatibility reasons, a system inherits from module and is a child-component
;; so we must guard against this case. ASDF3: remove that.
(defmethod component-depends-on ((o upward-operation) (c child-component))
  `(,@(aif (component-parent c) `((,o ,it))) ,@(call-next-method)))


;;;; Inputs, Outputs, and invisible dependencies
(defgeneric* output-files (operation component))
(defgeneric* input-files (operation component))
(defgeneric* operation-done-p (operation component)
  (:documentation "Returns a boolean, which is NIL if the action is forced to be performed again"))

(defmethod operation-done-p ((o operation) (c component))
  (declare (ignorable o c))
  t)

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


;;;; File components

(defclass file-component (child-component)
  ((type :accessor file-type :initarg :type))) ; no default
(defclass source-file (file-component)
  ((type :initform nil))) ;; NB: many systems have come to rely on this default.
(defclass c-source-file (source-file)
  ((type :initform "c")))
(defclass java-source-file (source-file)
  ((type :initform "java")))
(defclass static-file (source-file)
  ((type :initform nil)))
(defclass doc-file (static-file) ())
(defclass html-file (doc-file)
  ((type :initform "html")))

(defmethod input-files ((o operation) (c file-component))
  (or (loop :for (dep-o) :in (component-self-dependencies o c)
            :append (or (output-files dep-o c) (input-files dep-o c)))
      ;; no non-trivial previous operations needed?
      ;; I guess we work with the original source file, then
      (list (component-pathname c))))

(defmethod source-file-type ((component parent-component) system) ; not just for source-file. ASDF3: rename.
  (declare (ignorable component system))
  :directory)
(defmethod source-file-type ((component file-component) system)
  (declare (ignorable system))
  (file-type component))


;;;; Done performing

(defgeneric* component-operation-time (operation component)) ;; ASDF3: hide it behind plan-action-stamp
(defgeneric* mark-operation-done (operation component)) ;; ASDF3: hide it behind (setf plan-action-stamp)
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

(defmethod component-operation-time ((o operation) (c component))
  (gethash (type-of o) (component-operation-times c)))

(defmethod mark-operation-done ((o operation) (c component))
  (setf (gethash (type-of o) (component-operation-times c))
        (compute-action-stamp nil o c :just-done t)))


;;;; Perform

(defgeneric* perform-with-restarts (operation component))
(defgeneric* perform (operation component))

(defmethod perform :before ((o operation) (c component))
  (ensure-all-directories-exist (output-files o c)))
(defmethod perform :after ((o operation) (c component))
  (mark-operation-done o c))
(defmethod perform ((o operation) (c parent-component))
  (declare (ignorable o c))
  nil)
(defmethod perform ((o operation) (c source-file))
  (sysdef-error
   (compatfmt "~@<Required method PERFORM not implemented for operation ~A, component ~A~@:>")
   (class-of o) (class-of c)))

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

