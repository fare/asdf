;;;; -------------------------------------------------------------------------
;;;; Actions

(asdf/package:define-package :asdf/action
  (:nicknames :asdf-action)
  (:recycle :asdf/action :asdf)
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation)
  (:intern #:stamp #:done-p)
  (:export
   #:action #:define-convenience-action-methods
   #:explain #:operation-description
   #:downward-operation #:upward-operation #:sibling-operation
   #:component-depends-on #:component-self-dependencies
   #:input-files #:output-files #:output-file #:operation-done-p
   #:action-status #:action-stamp #:action-done-p
   #:component-operation-time #:mark-operation-done #:compute-action-stamp
   #:perform #:perform-with-restarts #:retry #:accept #:feature
   #:traverse-actions #:traverse-sub-actions #:required-components ;; in plan
   #:action-path #:find-action
   ))
(in-package :asdf/action)

(deftype action () '(cons operation component)) ;; a step to be performed while building the system

(defgeneric* traverse-actions (actions &key &allow-other-keys))
(defgeneric* traverse-sub-actions (operation component &key &allow-other-keys))
(defgeneric* required-components (component &key &allow-other-keys))

;;;; Reified representation for storage or debugging. Note: dropping original-initags
(defun action-path (action)
  (destructuring-bind (o . c) action (cons (type-of o) (component-find-path c))))
(defun find-action (path)
  (destructuring-bind (o . c) path (cons (make-operation o) (find-component () c))))


;;;; Convenience methods
(defmacro define-convenience-action-methods
  (function (operation component &optional keyp)
   &key if-no-operation if-no-component operation-initargs)
  (let* ((rest (gensym "REST"))
         (found (gensym "FOUND"))
         (more-args (when keyp `(&rest ,rest &key &allow-other-keys))))
    (flet ((next-method (o c)
             (if keyp
                 `(apply ',function ,o ,c ,rest)
                 `(,function ,o ,c))))
      `(progn
         (defmethod ,function ((,operation symbol) ,component ,@more-args)
           (if ,operation
               ,(next-method
                 (if operation-initargs ;backward-compatibility with ASDF1's operate. Yuck.
                     `(apply 'make-operation ,operation :original-initargs ,rest ,rest)
                     `(make-operation ,operation))
                 `(find-component () ,component))
               ,if-no-operation))
         (defmethod ,function ((,operation operation) ,component ,@more-args)
           (if (typep ,component 'component)
               (error "No defined method for ~S on ~S" ',function ,component)
               (let ((,found (find-component () ,component)))
                 (if ,found
                     ,(next-method operation found)
                     ,if-no-component))))))))


;;;; self-description

(defgeneric* operation-description (operation component) ;; ASDF3: rename to action-description
  (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))
(defmethod operation-description (operation component)
  (format nil (compatfmt "~@<~A on ~A~@:>")
          (type-of operation) component))
(defgeneric* (explain) (operation component))
(defmethod explain ((o operation) (c component))
  (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (operation-description o c)))
(define-convenience-action-methods explain (operation component))

(defun* format-action (stream action &optional colon-p at-sign-p)
  (assert (null colon-p)) (assert (null at-sign-p))
  (destructuring-bind (operation . component) action
    (princ (operation-description operation component) stream)))


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
(define-convenience-action-methods component-depends-on (operation component))
(define-convenience-action-methods component-self-dependencies (operation component))

(defmethod component-depends-on ((o operation) (c component))
  (cdr (assoc (type-of o) (component-in-order-to c)))) ; User-specified in-order dependencies

(defmethod component-self-dependencies ((o operation) (c component))
  ;; NB: result in the same format as component-depends-on
  (loop* :for (o-spec . c-spec) :in (component-depends-on o c)
         :unless (eq o-spec 'feature) ;; avoid the FEATURE "feature"
           :when (find c c-spec :key #'(lambda (dep) (resolve-dependency-spec c dep)))
             :collect (list o-spec c)))

;;;; upward-operation, downward-operation
;; These together handle actions that propagate along the component hierarchy.
;; Downward operations like load-op or compile-op propagate down the hierarchy:
;; operation on a parent depends-on operation on its children.
;; By default, an operation propagates itself, but it may propagate another one instead.
(defclass downward-operation (operation)
  ((downward-operation
    :initform nil :initarg :downward-operation :reader downward-operation)))
(defmethod component-depends-on ((o downward-operation) (c parent-component))
  `((,(or (downward-operation o) o) ,@(component-children c)) ,@(call-next-method)))
;; Upward operations like prepare-op propagate up the component hierarchy:
;; operation on a child depends-on operation on its parent.
;; By default, an operation propagates itself, but it may propagate another one instead.
(defclass upward-operation (operation)
  ((upward-operation
    :initform nil :initarg :downward-operation :reader upward-operation)))
;; For backward-compatibility reasons, a system inherits from module and is a child-component
;; so we must guard against this case. ASDF3: remove that.
(defmethod component-depends-on ((o upward-operation) (c child-component))
  `(,@(if-let (p (component-parent c))
        `((,(or (upward-operation o) o) ,p))) ,@(call-next-method)))
;; Sibling operations propagate to siblings in the component hierarchy:
;; operation on a child depends-on operation on its parent.
;; By default, an operation propagates itself, but it may propagate another one instead.
(defclass sibling-operation (operation)
  ((sibling-operation
    :initform nil :initarg :sibling-operation :reader sibling-operation)))
(defmethod component-depends-on ((o sibling-operation) (c component))
  `((,(or (sibling-operation o) o)
     ,@(loop :for dep :in (component-sibling-dependencies c)
             :collect (resolve-dependency-spec c dep)))
    ,@(call-next-method)))


;;;; Inputs, Outputs, and invisible dependencies
(defgeneric* (output-files) (operation component))
(defgeneric* (input-files) (operation component))
(defgeneric* (operation-done-p) (operation component)
  (:documentation "Returns a boolean, which is NIL if the action is forced to be performed again"))
(define-convenience-action-methods output-files (operation component))
(define-convenience-action-methods input-files (operation component))
(define-convenience-action-methods operation-done-p (operation component))

(defmethod operation-done-p ((o operation) (c component))
  (declare (ignorable o c))
  t)

(defmethod output-files :around (operation component)
  "Translate output files, unless asked not to"
  operation component ;; hush genera, not convinced by declare ignorable(!)
  (values
   (multiple-value-bind (pathnames fixedp) (call-next-method)
     ;; 1- Make sure we have absolute pathnames
     (let* ((directory (pathname-directory-pathname
                        (component-pathname (find-component () component))))
            (absolute-pathnames
              (loop
                :for pathname :in pathnames
                :collect (ensure-pathname-absolute pathname directory))))
       ;; 2- Translate those pathnames as required
       (if fixedp
           absolute-pathnames
           (mapcar *output-translation-function* absolute-pathnames))))
   t))
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

(defmethod input-files ((o operation) (c component))
  (or (loop* :for (dep-o) :in (component-self-dependencies o c)
             :append (or (output-files dep-o c) (input-files dep-o c)))
      ;; no non-trivial previous operations needed?
      ;; I guess we work with the original source file, then
      (if-let ((pathname (component-pathname c)))
        (and (file-pathname-p pathname) (list pathname)))))


;;;; Done performing

(defgeneric* component-operation-time (operation component)) ;; ASDF3: hide it behind plan-action-stamp
(define-convenience-action-methods component-operation-time (operation component))


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

(defgeneric* (perform-with-restarts) (operation component))
(defgeneric* (perform) (operation component))
(define-convenience-action-methods perform (operation component))

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

;;; Generic build operation
(defmethod component-depends-on ((o build-op) (c component))
  `((,(or (component-build-operation c) 'load-op) ,c)))

