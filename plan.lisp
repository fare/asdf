;;;; -------------------------------------------------------------------------
;;;; Plan

(uiop/package:define-package :asdf/plan
  ;; asdf/action below is needed for required-components, traverse-action and traverse-sub-actions
  ;; that used to live there before 3.2.0.
  (:recycle :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
        :asdf/component :asdf/operation :asdf/action :asdf/lisp-action
        :asdf/system :asdf/system-registry :asdf/find-component)
  (:export
   #:component-operation-time
   #:plan #:plan-traversal #:sequential-plan #:*plan-class*
   #:action-status #:action-stamp #:action-done-p #:action-index #:action-planned-p
   #:action-already-done-p #:+action-status-out-of-date+
   #:circular-dependency #:circular-dependency-actions
   #:needed-in-image-p
   #:record-dependency
   #:normalize-forced-systems #:action-forced-p #:action-forced-not-p
   #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies
   #:compute-action-stamp #:traverse-action
   #:make-plan #:plan-actions #:perform-plan #:plan-operates-on-p
   #:planned-p #:index #:forced #:forced-not
   #:plan-actions-r
   #:required-components #:filtered-sequential-plan
   #:plan-system
   #:plan-component-type #:plan-keep-operation #:plan-keep-component
   ))
(in-package :asdf/plan)

;;;; Generic plan traversal class
(with-upgradability ()
  (defclass plan () ()
    (:documentation "Base class for a plan based on which ASDF can build a system"))
  (defclass plan-traversal (plan)
    (;; Can this plan be performed? A plan that has different force and force-not settings than the
     ;; session plan can only be used for read-only queries that do not cause the status of any
     ;; action to be raised.
     (performable-p :initform nil :initarg :performable-p :reader plan-performable-p)
     ;; The parent plan from which this plan inherits forced status, etc.
     (parent :initform nil :initarg :parent :reader plan-parent)
     ;; The system for which the plan is computed
     (system :initform nil :initarg :system :accessor plan-system)
     ;; Table of systems specified via :force arguments
     (forced :initform nil :initarg :force :accessor forced)
     ;; Table of systems specified via :force-not argument (and/or immutable)
     (forced-not :initform nil :initarg :force-not :accessor forced-not))
    (:documentation "Base class for plans that simply traverse dependencies")))


;;;; Planned action status
(with-upgradability ()
  (defclass action-status ()
    ((stamp
      :initarg :stamp :reader action-stamp
      :documentation "STAMP associated with the ACTION if it has been completed already
in some previous image, or T if it needs to be done.")
     (done-p
      :initarg :done-p :reader action-done-p
      :documentation "a boolean, true iff the action was already done (before any planned action).")
     (planned-p
      :initarg :planned-p :initform nil :reader action-planned-p
      :documentation "a boolean, true iff the action was included in the plan.")
     (index
      :initarg :index :initform nil :reader action-index
      :documentation "an integer, counting all traversed actions in traversal order, or NIL."))
    (:documentation "Status of an action in a plan"))

  (defparameter +action-status-out-of-date+ (make-instance 'action-status :stamp t :done-p nil))

  (defun make-action-status (&key stamp done-p planned-p index)
    (if (eq stamp t)
        +action-status-out-of-date+
        (make-instance 'action-status :stamp stamp :done-p done-p :planned-p planned-p :index index)))

  (defmethod print-object ((status action-status) stream)
    (print-unreadable-object (status stream :type t)
      (with-slots (stamp done-p planned-p index) status
        (format stream "~{~S~^ ~}"
                `(:stamp ,stamp :done-p ,done-p
                         ,@(when planned-p `(:planned-p ,planned-p))
                         ,@(when index `(:index ,index)))))))

  (defgeneric action-status (plan operation component)
    (:documentation "Returns the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defgeneric (setf action-status) (new-status plan operation component)
    (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defun action-already-done-p (plan operation component)
    "According to this plan, is this action already done and up to date?"
    (action-done-p (action-status plan operation component)))

  (defmethod action-status ((plan null) (o operation) (c component))
    (multiple-value-bind (stamp done-p) (component-operation-time o c)
      (make-action-status :stamp stamp :done-p done-p)))

  (defmethod (setf action-status) (new-status (plan null) (o operation) (c component))
    (let ((times (component-operation-times c)))
      (if (action-done-p new-status)
          (remhash o times)
          (setf (gethash o times) (action-stamp new-status))))
    new-status))

;;;; forcing
(with-upgradability ()
  (defgeneric action-forced-p (plan operation component)
    (:documentation "Is this action forced to happen in this plan?"))
  (defgeneric action-forced-not-p (plan operation component)
    (:documentation "Is this action forced to not happen in this plan?
Takes precedence over action-forced-p."))

  (defun normalize-forced-systems (force system)
    "Given a SYSTEM on which operate is called and the specified FORCE argument,
extract a hash-set of systems that are forced, or a predicate on system names,
or NIL if none are forced, or :ALL if all are."
    (etypecase force
      ((or (member nil :all) hash-table function) force)
      (cons (list-to-hash-set (mapcar #'coerce-name force)))
      ((eql t) (when system (list-to-hash-set (list (coerce-name system)))))))

  (defun normalize-forced-not-systems (force-not system)
    "Given a SYSTEM on which operate is called, the specified FORCE-NOT argument,
and the set of IMMUTABLE systems, extract a hash-set of systems that are effectively forced-not,
or predicate on system names, or NIL if none are forced, or :ALL if all are."
    (let ((requested
            (etypecase force-not
              ((or (member nil :all) hash-table function) force-not)
              (cons (list-to-hash-set (mapcar #'coerce-name force-not)))
              ((eql t) (if system (let ((name (coerce-name system)))
                                    #'(lambda (x) (not (equal x name))))
                           :all)))))
      (if (and *immutable-systems* requested)
          #'(lambda (x) (or (call-function requested x)
                            (call-function *immutable-systems* x)))
          (or *immutable-systems* requested))))

  ;; TODO: shouldn't we be looking up the primary system name, rather than the system name?
  (defun action-override-p (plan operation component override-accessor)
    "Given a plan, an action, and a function that given the plan accesses a set of overrides
(i.e. force or force-not), see if the override applies to the current action."
    (declare (ignore operation))
    (call-function (funcall override-accessor plan)
                   (coerce-name (component-system (find-component () component)))))

  (defmethod action-forced-p (plan operation component)
    (and
     ;; Did the user ask us to re-perform the action?
     (action-override-p plan operation component 'forced)
     ;; You really can't force a builtin system and :all doesn't apply to it,
     ;; except if it's the specifically the system currently being built.
     (not (let ((system (component-system component)))
            (and (builtin-system-p system)
                 (not (eq system (plan-system plan))))))))

  (defmethod action-forced-not-p (plan operation component)
    ;; Did the user ask us to not re-perform the action?
    ;; NB: force-not takes precedence over force, as it should
    (action-override-p plan operation component 'forced-not))

  (defmethod action-forced-p ((plan null) (operation operation) (component component))
    nil)

  (defmethod action-forced-not-p ((plan null) (operation operation) (component component))
    nil))


;;;; Is the action needed in this image?
(with-upgradability ()
  (defgeneric needed-in-image-p (operation component)
    (:documentation "Is the action of OPERATION on COMPONENT needed in the current image
to be meaningful, or could it just as well have been done in another Lisp image?"))

  (defmethod needed-in-image-p ((o operation) (c component))
    ;; We presume that actions that modify the filesystem don't need be run
    ;; in the current image if they have already been done in another,
    ;; and can be run in another process (e.g. a fork),
    ;; whereas those that don't are meant to side-effect the current image and can't.
    (not (output-files o c))))


;;;; Visiting dependencies of an action and computing action stamps
(with-upgradability ()
  (defun* (map-direct-dependencies) (operation component fun)
    "Call FUN on all the valid dependencies of the given action in the given plan"
    (loop* :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
      :for dep-o = (find-operation operation dep-o-spec)
      :when dep-o
      :do (loop :for dep-c-spec :in dep-c-specs
            :for dep-c = (and dep-c-spec (resolve-dependency-spec component dep-c-spec))
            :when (action-valid-p dep-o dep-c)
            :do (funcall fun dep-o dep-c))))

  (defun* (reduce-direct-dependencies) (operation component combinator seed)
    "Reduce the direct dependencies to a value computed by iteratively calling COMBINATOR
for each dependency action on the dependency's operation and component and an accumulator
initialized with SEED."
    (map-direct-dependencies
     operation component
     #'(lambda (dep-o dep-c) (setf seed (funcall combinator dep-o dep-c seed))))
    seed)

  (defun* (direct-dependencies) (operation component)
    "Compute a list of the direct dependencies of the action within the plan"
    (reverse (reduce-direct-dependencies operation component #'acons nil)))

  ;; In a distant future, get-file-stamp, component-operation-time and latest-stamp
  ;; shall also be parametrized by the plan, or by a second model object,
  ;; so they need not refer to the state of the filesystem,
  ;; and the stamps could be cryptographic checksums rather than timestamps.
  ;; Such a change remarkably would only affect COMPUTE-ACTION-STAMP.

  (defmethod compute-action-stamp (plan (o operation) (c component) &key just-done)
    ;; Given an action, figure out at what time in the past it has been done,
    ;; or if it has just been done, return the time that it has.
    ;; Returns two values:
    ;; 1- the TIMESTAMP of the action if it has already been done and is up to date,
    ;;   or T is either hasn't been done or is out of date.
    ;; 2- the DONE-IN-IMAGE-P boolean flag that is T if the action has already been done
    ;;   in the current image, or NIL if it hasn't.
    ;; Note that if e.g. LOAD-OP only depends on up-to-date files, but
    ;; hasn't been done in the current image yet, then it can have a non-T timestamp,
    ;; yet a NIL done-in-image-p flag: we can predict what timestamp it will have once loaded,
    ;; i.e. that of the input-files.
    (nest
     (block ())
     (let ((dep-stamp ; collect timestamp from dependencies (or T if forced or out-of-date)
             (reduce-direct-dependencies
              o c
              #'(lambda (o c stamp)
                  (if-let (it (action-status plan o c))
                    (latest-stamp stamp (action-stamp it))
                    t))
              nil)))
       ;; out-of-date dependency: don't bother expensively querying the filesystem
       (when (and (eq dep-stamp t) (not just-done)) (return (values t nil))))
     ;; collect timestamps from inputs, and exit early if any is missing
     (let* ((in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (stamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done)) (return (values t nil))))
     ;; collect timestamps from outputs, and exit early if any is missing
     (let* ((out-files (remove-if 'null (output-files o c)))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (stamps-earliest out-stamps)))
       (when (and missing-out (not just-done)) (return (values t nil))))
     (let* (;; There are three kinds of actions:
            (out-op (and out-files t)) ; those that create files on the filesystem
            ;;(image-op (and in-files (null out-files))) ; those that load stuff into the image
            ;;(null-op (and (null out-files) (null in-files))) ; placeholders that do nothing
            ;; When was the thing last actually done? (Now, or ask.)
            (op-time (or just-done (component-operation-time o c)))
            ;; Time stamps from the files at hand, and whether any is missing
            (all-present (not (or missing-in missing-out)))
            ;; Has any input changed since we last generated the files?
            (up-to-date-p (stamp<= latest-in earliest-out))
            ;; If everything is up to date, the latest of inputs and outputs is our stamp
            (done-stamp (stamps-latest (cons latest-in out-stamps))))
       ;; Warn if some files are missing:
       ;; either our model is wrong or some other process is messing with our files.
       (when (and just-done (not all-present))
         (warn "~A completed without ~:[~*~;~*its input file~:p~2:*~{ ~S~}~*~]~
                ~:[~; or ~]~:[~*~;~*its output file~:p~2:*~{ ~S~}~*~]"
               (action-description o c)
               missing-in (length missing-in) (and missing-in missing-out)
               missing-out (length missing-out))))
     ;; Note that we use stamp<= instead of stamp< to play nice with generated files.
     ;; Any race condition is intrinsic to the limited timestamp resolution.
     (if (or just-done ;; The done-stamp is valid: if we're just done, or
             ;; if all filesystem effects are up-to-date and there's no invalidating reason.
             (and all-present up-to-date-p (operation-done-p o c) (not (action-forced-p plan o c))))
         (values done-stamp ;; return the hard-earned timestamp
                 (or just-done
                     out-op ;; a file-creating op is done when all files are up to date
                     ;; a image-effecting a placeholder op is done when it was actually run,
                     (and op-time (eql op-time done-stamp)))) ;; with the matching stamp
         ;; done-stamp invalid: return a timestamp in an indefinite future, action not done yet
         (values t nil)))))


;;;; Generic support for plan-traversal
(with-upgradability ()
  (defmethod initialize-instance :after ((plan plan-traversal)
                                         &key force force-not system
                                         &allow-other-keys)
    (with-slots (forced forced-not) plan
      (setf forced (normalize-forced-systems force system))
      (setf forced-not (normalize-forced-not-systems force-not system))))

  (defgeneric plan-actions (plan)
    (:documentation "Extract from a plan a list of actions to perform in sequence"))
  (defmethod plan-actions ((plan list))
    plan)

  (defmethod (setf action-status) (new-status (p plan) (o operation) (c component))
    (setf (gethash (cons o c) (visited-actions *asdf-session*)) new-status))

  (defmethod action-status ((p plan) (o operation) (c component))
    (if (action-forced-not-p p o c)
        (action-status nil o c)
        (values (gethash (cons o c) (visited-actions *asdf-session*)))))

  (defgeneric record-dependency (plan operation component)
    (:documentation "Record an action as a dependency in the current plan")))


;;;; Actual traversal: traverse-action
(with-upgradability ()
  (defgeneric traverse-action (plan operation component needed-in-image-p))

  ;; TRAVERSE-ACTION, in the context of a given PLAN object that accumulates dependency data,
  ;; visits the action defined by its OPERATION and COMPONENT arguments,
  ;; and all its transitive dependencies (unless already visited),
  ;; in the context of the action being (or not) NEEDED-IN-IMAGE-P,
  ;; i.e. needs to be done in the current image vs merely have been done in a previous image.
  ;; For actions that are up-to-date, it returns a STAMP identifying the state of the action
  ;; (that's timestamp, but it could be a cryptographic digest in some ASDF extension),
  ;; or T if the action needs to be done again.
  ;;
  ;; Note that for an XCVB-like plan with one-image-per-file-outputting-action,
  ;; the below method would be insufficient, since it assumes a single image
  ;; to traverse each node at most twice; non-niip actions would be traversed only once,
  ;; but niip nodes could be traversed once per image, i.e. once plus once per non-niip action.

  ;; Handle FORCED-NOT: it makes an action return its current timestamp as status
  (defmethod action-status :around ((plan plan) operation component)
    ;; TODO: should we instead test something like:
    ;; (action-forced-not-p plan operation (primary-system component))
    (if (action-forced-not-p plan operation component)
        (let ((status (action-status nil operation component)))
          (if (and status (action-done-p status))
              status
              (let ((stamp (and status (action-stamp status))))
                (make-action-status :done-p t :stamp stamp))))
        (call-next-method)))

  ;; Different traversals:
  ;; * Get the dependencies (filtered), don't change any status
  ;; * Check whether some (defsystem-depends-on ?) dependencies are up to date
  ;; * Check that some dependencies are up-to-date, though not necessarily in image
  ;; * Check that some dependencies are up-to-date and in image, or add them to the plan

  (defun action-up-to-date-p (plan operation component)
    "Check whether a node is up-to-date, and mark it so if such. But don't add anything to the plan."
    (while-visiting-action (operation component) ; maintain context, handle circularity.
      (block nil
        ;; Do NOT record the dependency: it might be out of date.
        (let ((status (action-status plan operation component)))
          (when status
            (return (not (eq (action-stamp status) t)))))
        (let* ((dependencies-up-to-date-p
                (block nil
                  (map-direct-dependencies ; recursively traverse dependencies
                   operation component
                   #'(lambda (o c) (unless (action-up-to-date-p plan o c) (return nil))))
                  t))
               (status
                (if dependencies-up-to-date-p
                    ;; If it's all up-to-date
                    (multiple-value-bind (stamp done-p)
                        (compute-action-stamp plan operation component) ; compute action stamp TODO: no plan!
                      (make-action-status :stamp stamp :done-p done-p))
                    +action-status-out-of-date+)))
          (setf (action-status plan operation component) status)
          (not (eq (action-stamp status) t))))))

  (defmethod traverse-action (plan operation component needed-in-image-p)
    (while-visiting-action (operation component) ; maintain context, handle circularity.
      (block nil
        ;; Record the dependency. This hook is needed by POIU, which tracks a full dependency graph,
        ;; instead of just a dependency order as in vanilla ASDF.
        ;; TODO: It is also needed to detect OPERATE-in-PERFORM.
        (record-dependency plan operation component)
        ;; needed-in-image distinguishes b/w things that must happen in the
        ;; current image and those things that simply need to have been done in a previous one.
        (let* ((aniip (needed-in-image-p operation component)) ; action-specific needed-in-image
               ;; effective niip: meaningful for the action and required by the plan as traversed
               (eniip (and aniip needed-in-image-p))
               ;; status: have we traversed that action previously, and if so what was its status?
               (status (action-status plan operation component)))
          (when (and status (or (action-done-p status) (action-planned-p status) (not eniip)))
            (return (action-stamp status))) ; Already visited with sufficient need-in-image level!
          (labels ((visit-action (niip) ; We may visit the action twice, once with niip NIL, then T
                     (map-direct-dependencies ; recursively traverse dependencies
                      operation component #'(lambda (o c) (traverse-action plan o c niip)))
                     (multiple-value-bind (stamp done-p) ; AFTER dependencies have been traversed,
                         (compute-action-stamp plan operation component) ; compute action stamp
                       (let ((add-to-plan-p (or (eql stamp t) (and niip (not done-p)))))
                         (cond ; it needs be done if it's out of date or needed in image but absent
                           ((and add-to-plan-p (not niip)) ; if we need to do it,
                            (visit-action t)) ; then we need to do it *in the (current) image*!
                           (t
                            (setf (action-status plan operation component) ; update status:
                                  (make-instance
                                   'action-status
                                   :stamp stamp ; computed stamp
                                   :done-p (and done-p (not add-to-plan-p)) ; done *and* up-to-date?
                                   :planned-p add-to-plan-p ; included in list of things to be done?
                                   :index (if status ;; index of action among all nodes in traversal
                                              (action-index status) ;; keep index if already visited
                                               ;; else allocate a new session-wide index
                                              (incf (total-action-count *asdf-session*)))))
                            (when (and done-p (not add-to-plan-p))
                              (setf (component-operation-time operation component) stamp))
                            (when add-to-plan-p ; if it needs to be added to the plan,
                              (incf (planned-action-count *asdf-session*)) ; count it
                              (unless aniip ; if it's output-producing,
                                (incf (planned-output-action-count *asdf-session*)))) ; count it
                            stamp)))))) ; return the stamp
            (visit-action eniip))))))) ; visit the action


;;;; Sequential plans (the default)
(with-upgradability ()
  (defclass sequential-plan (plan-traversal)
    ((actions-r :initform nil :accessor plan-actions-r))
    (:documentation "Simplest, default plan class, accumulating a sequence of actions"))

  (defmethod plan-actions ((plan sequential-plan))
    (reverse (plan-actions-r plan)))

  ;; No need to record a dependency to build a full graph, just accumulate nodes in order.
  (defmethod record-dependency ((plan sequential-plan) (o operation) (c component))
    (values))

  (defmethod (setf action-status) :after
      (new-status (p sequential-plan) (o operation) (c component))
    (when (action-planned-p new-status)
      (push (make-action o c) (plan-actions-r p)))))


;;;; High-level interface: traverse, perform-plan, plan-operates-on-p
(with-upgradability ()
  (defgeneric make-plan (plan-class operation component &key &allow-other-keys)
    (:documentation "Generate and return a plan for performing OPERATION on COMPONENT."))
  (define-convenience-action-methods make-plan (plan-class operation component &key))

  (defgeneric perform-plan (plan &key)
    (:documentation "Actually perform a plan and build the requested actions"))
  (defgeneric plan-operates-on-p (plan component)
    (:documentation "Does this PLAN include any operation on given COMPONENT?"))

  (defparameter* *plan-class* 'sequential-plan
    "The default plan class to use when building with ASDF")

  (defmethod make-plan (plan-class (o operation) (c component) &rest keys &key &allow-other-keys)
    (with-asdf-session ()
      (let ((plan (apply 'make-instance (or plan-class *plan-class*)
                         :system (component-system c) keys)))
        (traverse-action plan o c t)
        plan)))

  (defmethod perform-plan :around ((plan t) &key)
    (let ((*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit () ;; backward-compatibility.
        (call-next-method))))   ;; Going forward, see deferred-warning support in lisp-build.

  (defmethod perform-plan ((plan t) &key &allow-other-keys)
    (loop :for action :in (plan-actions plan)
      :as o = (action-operation action)
      :as c = (action-component action) :do
      (unless (nth-value 1 (compute-action-stamp plan o c))
        (perform-with-restarts o c))))

  (defmethod plan-operates-on-p ((plan plan-traversal) (component-path list))
    (plan-operates-on-p (plan-actions plan) component-path))

  (defmethod plan-operates-on-p ((plan list) (component-path list))
    (find component-path (mapcar 'action-component plan)
          :test 'equal :key 'component-find-path)))


;;;; Incidental traversals

;;; Making a FILTERED-SEQUENTIAL-PLAN can be used to, e.g., all of the source
;;; files required by a bundling operation.
(with-upgradability ()
  (defclass filtered-sequential-plan (sequential-plan)
    ((component-type :initform t :initarg :component-type :reader plan-component-type)
     (keep-operation :initform t :initarg :keep-operation :reader plan-keep-operation)
     (keep-component :initform t :initarg :keep-component :reader plan-keep-component))
    (:documentation "A variant of SEQUENTIAL-PLAN that only records a subset of actions."))

  (defmethod initialize-instance :after ((plan filtered-sequential-plan)
                                         &key force force-not
                                         other-systems)
    (declare (ignore force force-not))
    ;; Ignore force and force-not, rely on other-systems:
    ;; force traversal of what we're interested in, i.e. current system or also others;
    ;; force-not traversal of what we're not interested in, i.e. other systems unless other-systems.
    (with-slots (forced forced-not system) plan
      (setf forced :all)
      (setf forced-not (normalize-forced-not-systems (if other-systems nil t) system))))

  (defmethod plan-actions ((plan filtered-sequential-plan))
    (with-slots (keep-operation keep-component) plan
      (loop :for action :in (call-next-method)
        :as o = (action-operation action)
        :as c = (action-component action)
        :when (and (typep o keep-operation) (typep c keep-component))
        :collect (cons o c))))

  (defun collect-action-dependencies (plan operation component)
    (while-visiting-action (operation component) ; maintain context, handle circularity.
      (let ((action (make-action operation component)))
        (unless (gethash action (visited-actions *asdf-session*))
          (setf (gethash action (visited-actions *asdf-session*)) t)
          (when (and (typep component (plan-component-type plan))
                     (not (action-forced-not-p plan operation component)))
            (map-direct-dependencies operation component
                                     #'(lambda (o c) (collect-action-dependencies plan o c)))
            (push action (plan-actions-r plan)))))))

  (defgeneric collect-dependencies (operation component &key &allow-other-keys)
    (:documentation "Given an action, build a plan for all of its dependencies."))
  (define-convenience-action-methods collect-dependencies (operation component &key))
  (defmethod collect-dependencies ((operation operation) (component component)
                                   &rest keys &key &allow-other-keys)
    (let ((plan (apply 'make-instance 'filtered-sequential-plan
                       :system (component-system component) :force :all keys)))
      (loop :for action :in (direct-dependencies operation component)
        :do (collect-action-dependencies plan (action-operation action) (action-component action)))
      (plan-actions plan)))

  (defun* (required-components) (system &rest keys &key (goal-operation 'load-op) &allow-other-keys)
    "Given a SYSTEM and a GOAL-OPERATION (default LOAD-OP), traverse the dependencies and
return a list of the components involved in building the desired action."
    (let ((cache (and *asdf-session* (session-cache *asdf-session*))))
      (with-asdf-session (:override t)
        (when cache (setf (session-cache *asdf-session*) cache))
        (remove-duplicates
         (mapcar 'action-component
                 (apply 'collect-dependencies goal-operation system
                        (remove-plist-key :goal-operation keys)))
         :from-end t)))))

