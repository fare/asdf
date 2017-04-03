;;;; -------------------------------------------------------------------------
;;;; Actions

(uiop/package:define-package :asdf/action
  (:nicknames :asdf-action)
  (:recycle :asdf/action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system #:asdf/cache :asdf/find-system :asdf/find-component :asdf/operation)
  (:import-from :asdf/operation #:check-operation-constructor)
  #-clisp (:unintern #:required-components #:traverse-action #:traverse-sub-actions)
  (:export
   #:action #:define-convenience-action-methods
   #:action-description
   #:downward-operation #:upward-operation #:sideway-operation #:selfward-operation #:non-propagating-operation
   #:component-depends-on
   #:input-files #:output-files #:output-file #:operation-done-p
   #:action-status #:action-stamp #:action-done-p
   #:action-operation #:action-component #:make-action
   #:component-operation-time #:mark-operation-done #:compute-action-stamp
   #:perform #:perform-with-restarts #:retry #:accept
   #:action-path #:find-action #:stamp #:done-p
   #:operation-definition-warning #:operation-definition-error ;; condition
   ))
(in-package :asdf/action)

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute) ;; LispWorks issues spurious warning

  (deftype action ()
    "A pair of operation and component uniquely identifies a node in the dependency graph
of steps to be performed while building a system."
    '(cons operation component))

  (deftype operation-designator ()
    "An operation designates itself. NIL designates a context-dependent current operation,
and a class-name or class designates the canonical instance of the designated class."
    '(or operation null symbol class)))

;;; these are pseudo accessors -- let us abstract away the CONS cell representation of plan
;;; actions.
(with-upgradability ()
  (defun make-action (operation component)
    (cons operation component))
  (defun action-operation (action)
    (car action))
  (defun action-component (action)
    (cdr action)))

;;;; Reified representation for storage or debugging. Note: an action is identified by its class.
(with-upgradability ()
  (defun action-path (action)
    "A readable data structure that identifies the action."
    (let ((o (action-operation action))
          (c (action-component action)))
      (cons (type-of o) (component-find-path c))))
  (defun find-action (path)
    "Reconstitute an action from its action-path"
    (destructuring-bind (o . c) path (make-action (make-operation o) (find-component () c)))))

;;;; Convenience methods
(with-upgradability ()
  ;; A macro that defines convenience methods for a generic function (gf) that
  ;; dispatches on operation and component.  The convenience methods allow users
  ;; to call the gf with operation and/or component designators, that the
  ;; methods will resolve into actual operation and component objects, so that
  ;; the users can interact using readable designators, but developers only have
  ;; to write methods that handle operation and component objects.
  ;; FUNCTION is the generic function name
  ;; FORMALS is its list of arguments, which must include OPERATION and COMPONENT.
  ;; IF-NO-OPERATION is a form (defaults to NIL) describing what to do if no operation is found.
  ;; IF-NO-COMPONENT is a form (defaults to NIL) describing what to do if no component is found.
  (defmacro define-convenience-action-methods
      (function formals &key if-no-operation if-no-component)
    (let* ((rest (gensym "REST"))
           (found (gensym "FOUND"))
           (keyp (equal (last formals) '(&key)))
           (formals-no-key (if keyp (butlast formals) formals))
           (len (length formals-no-key))
           (operation 'operation)
           (component 'component)
           (opix (position operation formals))
           (coix (position component formals))
           (prefix (subseq formals 0 opix))
           (suffix (subseq formals (1+ coix) len))
           (more-args (when keyp `(&rest ,rest &key &allow-other-keys))))
      (assert (and (integerp opix) (integerp coix) (= coix (1+ opix))))
      (flet ((next-method (o c)
               (if keyp
                   `(apply ',function ,@prefix ,o ,c ,@suffix ,rest)
                   `(,function ,@prefix ,o ,c ,@suffix))))
        `(progn
           (defmethod ,function (,@prefix (,operation string) ,component ,@suffix ,@more-args)
             (declare (notinline ,function))
             (let ((,component (find-component () ,component))) ;; do it first, for defsystem-depends-on
               ,(next-method `(safe-read-from-string ,operation :package :asdf/interface) component)))
           (defmethod ,function (,@prefix (,operation symbol) ,component ,@suffix ,@more-args)
             (declare (notinline ,function))
             (if ,operation
                 ,(next-method
                   `(make-operation ,operation)
                   `(or (find-component () ,component) ,if-no-component))
                 ,if-no-operation))
           (defmethod ,function (,@prefix (,operation operation) ,component ,@suffix ,@more-args)
             (declare (notinline ,function))
             (if (typep ,component 'component)
                 (error "No defined method for ~S on ~/asdf-action:format-action/"
                        ',function (make-action ,operation ,component))
                 (if-let (,found (find-component () ,component))
                    ,(next-method operation found)
                    ,if-no-component))))))))


;;;; self-description
(with-upgradability ()
  (defgeneric action-description (operation component)
    (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))
  (defmethod action-description (operation component)
    (format nil (compatfmt "~@<~A on ~A~@:>")
            operation component))

  (defun format-action (stream action &optional colon-p at-sign-p)
    "FORMAT helper to display an action's action-description.
Use it in FORMAT control strings as ~/asdf-action:format-action/"
    (assert (null colon-p)) (assert (null at-sign-p))
    (destructuring-bind (operation . component) action
      (princ (action-description operation component) stream))))


;;;; Dependencies
(with-upgradability ()
  (defgeneric component-depends-on (operation component) ;; ASDF4: rename to component-dependencies
    (:documentation
     "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is an operation designator
        with respect to FIND-OPERATION in the context of the OPERATION argument,
        and each <component> is a component designator with respect to
        FIND-COMPONENT in the context of the COMPONENT argument,
        and means that the component depends on
        <operation> having been performed on each <component>;

        [Note: an <operation> is an operation designator -- it can be either an
        operation name or an operation object.  Similarly, a <component> may be
        a component name or a component object.  Also note that, the degenerate
        case of (<operation>) is a no-op.]

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the list."))
  (define-convenience-action-methods component-depends-on (operation component))

  (defmethod component-depends-on :around ((o operation) (c component))
    (do-asdf-cache `(component-depends-on ,o ,c)
      (call-next-method))))


;;;; upward-operation, downward-operation, sideway-operation, selfward-operation
;; These together handle actions that propagate along the component hierarchy or operation universe.
(with-upgradability ()
  (defclass downward-operation (operation)
    ((downward-operation
      :initform nil :reader downward-operation
      :type operation-designator :allocation :class))
    (:documentation "A DOWNWARD-OPERATION's dependencies propagate down the component hierarchy.
I.e., if O is a DOWNWARD-OPERATION and its DOWNWARD-OPERATION slot designates operation D, then
the action (O . M) of O on module M will depends on each of (D . C) for each child C of module M.
The default value for slot DOWNWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a MODULE to be loaded with LOAD-OP (resp. compiled with COMPILE-OP), all the
children of the MODULE must have been loaded with LOAD-OP (resp. compiled with COMPILE-OP."))
  (defun downward-operation-depends-on (o c)
    `((,(or (downward-operation o) o) ,@(component-children c))))
  (defmethod component-depends-on ((o downward-operation) (c parent-component))
    `(,@(downward-operation-depends-on o c) ,@(call-next-method)))

  (defclass upward-operation (operation)
    ((upward-operation
      :initform nil :reader upward-operation
      :type operation-designator :allocation :class))
    (:documentation "An UPWARD-OPERATION has dependencies that propagate up the component hierarchy.
I.e., if O is an instance of UPWARD-OPERATION, and its UPWARD-OPERATION slot designates operation U,
then the action (O . C) of O on a component C that has the parent P will depends on (U . P).
The default value for slot UPWARD-OPERATION is NIL, which designates the operation O itself.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP, its PARENT
must first be prepared for loading or compiling with PREPARE-OP."))
  ;; For backward-compatibility reasons, a system inherits from module and is a child-component
  ;; so we must guard against this case. ASDF4: remove that.
  (defun upward-operation-depends-on (o c)
    (if-let (p (component-parent c)) `((,(or (upward-operation o) o) ,p))))
  (defmethod component-depends-on ((o upward-operation) (c child-component))
    `(,@(upward-operation-depends-on o c) ,@(call-next-method)))

  (defclass sideway-operation (operation)
    ((sideway-operation
      :initform nil :reader sideway-operation
      :type operation-designator :allocation :class))
    (:documentation "A SIDEWAY-OPERATION has dependencies that propagate \"sideway\" to siblings
that a component depends on. I.e. if O is a SIDEWAY-OPERATION, and its SIDEWAY-OPERATION slot
designates operation S (where NIL designates O itself), then the action (O . C) of O on component C
depends on each of (S . D) where D is a declared dependency of C.
E.g. in order for a COMPONENT to be prepared for loading or compiling with PREPARE-OP,
each of its declared dependencies must first be loaded as by LOAD-OP."))
  (defun sideway-operation-depends-on (o c)
    `((,(or (sideway-operation o) o) ,@(component-sideway-dependencies c))))
  (defmethod component-depends-on ((o sideway-operation) (c component))
    `(,@(sideway-operation-depends-on o c) ,@(call-next-method)))

  (defclass selfward-operation (operation)
    ((selfward-operation
      ;; NB: no :initform -- if an operation depends on others, it must explicitly specify which
      :type (or operation-designator list) :reader selfward-operation :allocation :class))
    (:documentation "A SELFWARD-OPERATION depends on another operation on the same component.
I.e., if O is a SELFWARD-OPERATION, and its SELFWARD-OPERATION designates a list of operations L,
then the action (O . C) of O on component C depends on each (S . C) for S in L.
E.g. before a component may be loaded by LOAD-OP, it must have been compiled by COMPILE-OP.
A operation-designator designates a singleton list of the designated operation;
a list of operation-designators designates the list of designated operations;
NIL is not a valid operation designator in that context.  Note that any dependency
ordering between the operations in a list of SELFWARD-OPERATION should be specified separately
in the respective operation's COMPONENT-DEPENDS-ON methods so that they be scheduled properly."))
  (defun selfward-operation-depends-on (o c)
    (loop :for op :in (ensure-list (selfward-operation o)) :collect `(,op ,c)))
  (defmethod component-depends-on ((o selfward-operation) (c component))
    `(,@(selfward-operation-depends-on o c) ,@(call-next-method)))

  (defclass non-propagating-operation (operation)
    ()
    (:documentation "A NON-PROPAGATING-OPERATION is an operation that propagates
no dependencies whatsoever.  It is supplied in order that the programmer be able
to specify that s/he is intentionally specifying an operation which invokes no
dependencies.")))


;;;---------------------------------------------------------------------------
;;; Help programmers catch obsolete OPERATION subclasses
;;;---------------------------------------------------------------------------
(with-upgradability ()
  (define-condition operation-definition-warning (simple-warning)
    ()
    (:documentation "Warning condition related to definition of obsolete OPERATION objects."))

  (define-condition operation-definition-error (simple-error)
    ()
    (:documentation "Error condition related to definition of incorrect OPERATION objects."))

  (defmethod initialize-instance :before ((o operation) &key)
    (check-operation-constructor)
    (unless (typep o '(or downward-operation upward-operation sideway-operation
                          selfward-operation non-propagating-operation))
      (warn 'operation-definition-warning
            :format-control
            "No dependency propagating scheme specified for operation class ~S.
The class needs to be updated for ASDF 3.1 and specify appropriate propagation mixins."
            :format-arguments (list (type-of o)))))

  (defmethod initialize-instance :before ((o non-propagating-operation) &key)
    (when (typep o '(or downward-operation upward-operation sideway-operation selfward-operation))
      (error 'operation-definition-error
             :format-control
             "Inconsistent class: ~S
  NON-PROPAGATING-OPERATION is incompatible with propagating operation classes as superclasses."
             :format-arguments
             (list (type-of o)))))

  (defun backward-compatible-depends-on (o c)
    "DEPRECATED: all subclasses of OPERATION used in ASDF should inherit from one of
 DOWNWARD-OPERATION UPWARD-OPERATION SIDEWAY-OPERATION SELFWARD-OPERATION NON-PROPAGATING-OPERATION.
 The function BACKWARD-COMPATIBLE-DEPENDS-ON temporarily provides ASDF2 behaviour for those that
 don't. In the future this functionality will be removed, and the default will be no propagation."
    (uiop/version::notify-deprecated-function
     (version-deprecation *asdf-version* :style-warning "3.2")
     'backward-compatible-depends-on)
    `(,@(sideway-operation-depends-on o c)
      ,@(when (typep c 'parent-component) (downward-operation-depends-on o c))))

  (defmethod component-depends-on ((o operation) (c component))
    `(;; Normal behavior, to allow user-specified in-order-to dependencies
      ,@(cdr (assoc (type-of o) (component-in-order-to c)))
        ;; For backward-compatibility with ASDF2, any operation that doesn't specify propagation
        ;; or non-propagation through an appropriate mixin will be downward and sideway.
        ,@(unless (typep o '(or downward-operation upward-operation sideway-operation
                             selfward-operation non-propagating-operation))
            (backward-compatible-depends-on o c))))

  (defmethod downward-operation ((o operation)) nil)
  (defmethod sideway-operation ((o operation)) nil))


;;;---------------------------------------------------------------------------
;;; End of OPERATION class checking
;;;---------------------------------------------------------------------------


;;;; Inputs, Outputs, and invisible dependencies
(with-upgradability ()
  (defgeneric output-files (operation component)
    (:documentation "Methods for this function return two values: a list of output files
corresponding to this action, and a boolean indicating if they have already been subjected
to relevant output translations and should not be further translated.

Methods on PERFORM *must* call this function to determine where their outputs are to be located.
They may rely on the order of the files to discriminate between outputs.
"))
  (defgeneric input-files (operation component)
    (:documentation "A list of input files corresponding to this action.

Methods on PERFORM *must* call this function to determine where their inputs are located.
They may rely on the order of the files to discriminate between inputs.
"))
  (defgeneric operation-done-p (operation component)
    (:documentation "Returns a boolean which is NIL if the action must be performed (again)."))
  (define-convenience-action-methods output-files (operation component))
  (define-convenience-action-methods input-files (operation component))
  (define-convenience-action-methods operation-done-p (operation component))

  (defmethod operation-done-p ((o operation) (c component))
    t)

  ;; Translate output files, unless asked not to. Memoize the result.
  (defmethod output-files :around ((operation t) (component t))
    (do-asdf-cache `(output-files ,operation ,component)
      (values
       (multiple-value-bind (pathnames fixedp) (call-next-method)
         ;; 1- Make sure we have absolute pathnames
         (let* ((directory (pathname-directory-pathname
                            (component-pathname (find-component () component))))
                (absolute-pathnames
                  (loop
                    :for pathname :in pathnames
                    :collect (ensure-absolute-pathname pathname directory))))
           ;; 2- Translate those pathnames as required
           (if fixedp
               absolute-pathnames
               (mapcar *output-translation-function* absolute-pathnames))))
       t)))
  (defmethod output-files ((o operation) (c component))
    nil)
  (defun output-file (operation component)
    "The unique output file of performing OPERATION on COMPONENT"
    (let ((files (output-files operation component)))
      (assert (length=n-p files 1))
      (first files)))

  ;; Memoize input files.
  (defmethod input-files :around (operation component)
    (do-asdf-cache `(input-files ,operation ,component)
      (call-next-method)))

  ;; By default an action has no input-files.
  (defmethod input-files ((o operation) (c component))
    nil)

  ;; An action with a selfward-operation by default gets its input-files from the output-files of
  ;; the actions using selfward-operations it depends on (and the same component),
  ;; or if there are none, on the component-pathname of the component if it's a file
  ;; -- and then on the results of the next-method.
  (defmethod input-files ((o selfward-operation) (c component))
    `(,@(or (loop :for dep-o :in (ensure-list (selfward-operation o))
                  :append (or (output-files dep-o c) (input-files dep-o c)))
            (if-let ((pathname (component-pathname c)))
              (and (file-pathname-p pathname) (list pathname))))
      ,@(call-next-method))))


;;;; Done performing
(with-upgradability ()
  ;; ASDF4: hide it behind plan-action-stamp
  (defgeneric component-operation-time (operation component)
    (:documentation "Return the timestamp for when an action was last performed"))
  (defgeneric (setf component-operation-time) (time operation component)
    (:documentation "Update the timestamp for when an action was last performed"))
  (define-convenience-action-methods component-operation-time (operation component))

  ;; ASDF4: hide it behind (setf plan-action-stamp)
  (defgeneric mark-operation-done (operation component)
    (:documentation "Mark a action as having been just done.

Updates the action's COMPONENT-OPERATION-TIME to match the COMPUTE-ACTION-STAMP
using the JUST-DONE flag."))
  (defgeneric compute-action-stamp (plan operation component &key just-done)
    (:documentation "Has this action been successfully done already,
and at what known timestamp has it been done at or will it be done at?
* PLAN is a plan object modelling future effects of actions,
  or NIL to denote what actually happened.
* OPERATION and COMPONENT denote the action.
Takes keyword JUST-DONE:
* JUST-DONE is a boolean that is true if the action was just successfully performed,
  at which point we want compute the actual stamp and warn if files are missing;
  otherwise we are making plans, anticipating the effects of the action.
Returns two values:
* a STAMP saying when it was done or will be done,
  or T if the action involves files that need to be recomputed.
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
    (gethash o (component-operation-times c)))

  (defmethod (setf component-operation-time) (stamp (o operation) (c component))
    (setf (gethash o (component-operation-times c)) stamp))

  (defmethod mark-operation-done ((o operation) (c component))
    (setf (component-operation-time o c) (compute-action-stamp nil o c :just-done t))))


;;;; Perform
(with-upgradability ()
  (defgeneric perform (operation component)
    (:documentation "PERFORM an action, consuming its input-files and building its output-files"))
  (define-convenience-action-methods perform (operation component))

  (defmethod perform :before ((o operation) (c component))
    (ensure-all-directories-exist (output-files o c)))
  (defmethod perform :after ((o operation) (c component))
    (mark-operation-done o c))
  (defmethod perform ((o operation) (c parent-component))
    nil)
  (defmethod perform ((o operation) (c source-file))
    ;; For backward compatibility, don't error on operations that don't specify propagation.
    (when (typep o '(or downward-operation upward-operation sideway-operation
                     selfward-operation non-propagating-operation))
      (sysdef-error
       (compatfmt "~@<Required method ~S not implemented for ~/asdf-action:format-action/~@:>")
       'perform (make-action o c))))

  ;; The restarts of the perform-with-restarts variant matter in an interactive context.
  ;; The retry strategies of p-w-r itself, and/or the background workers of a multiprocess build
  ;; may call perform directly rather than call p-w-r.
  (defgeneric perform-with-restarts (operation component)
    (:documentation "PERFORM an action in a context where suitable restarts are in place."))
  (defmethod perform-with-restarts (operation component)
    (perform operation component))
  (defmethod perform-with-restarts :around (operation component)
    (loop
      (restart-case
          (return (call-next-method))
        (retry ()
          :report
          (lambda (s)
            (format s (compatfmt "~@<Retry ~A.~@:>")
                    (action-description operation component))))
        (accept ()
          :report
          (lambda (s)
            (format s (compatfmt "~@<Continue, treating ~A as having been successful.~@:>")
                    (action-description operation component)))
          (mark-operation-done operation component)
          (return))))))
