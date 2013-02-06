;;;; -------------------------------------------------------------------------
;;;; Plan

(asdf/package:define-package :asdf/plan
  (:recycle :asdf/plan :asdf)
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/operation :asdf/system
   :asdf/cache :asdf/find-system :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action)
  (:export
   #:component-operation-time #:mark-operation-done
   #:plan-traversal #:sequential-plan #:*default-plan-class*
   #:planned-action-status #:plan-action-status #:action-already-done-p
   #:circular-dependency #:circular-dependency-actions
   #:node-for #:needed-in-image-p
   #:action-index #:action-planned-p #:action-valid-p
   #:plan-record-dependency #:visiting-action-p
   #:normalize-forced-systems #:action-forced-p #:action-forced-not-p
   #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies
   #:visit-dependencies #:compute-action-stamp #:traverse-action
   #:circular-dependency #:circular-dependency-actions
   #:call-while-visiting-action #:while-visiting-action
   #:traverse #:plan-actions #:perform-plan #:plan-operates-on-p
   #:planned-p #:index #:forced #:forced-not #:total-action-count
   #:planned-action-count #:planned-output-action-count #:visited-actions
   #:visiting-action-set #:visiting-action-list #:plan-actions-r
   #:required-components #:filtered-sequential-plan
   #:plan-system
   #:plan-action-filter #:plan-component-type #:plan-keep-operation #:plan-keep-component
   #:traverse-actions #:traverse-sub-actions))
(in-package :asdf/plan)

;;;; Generic plan traversal class
(with-upgradability ()
  (defclass plan-traversal ()
    ((system :initform nil :initarg :system :accessor plan-system)
     (forced :initform nil :initarg :force :accessor plan-forced)
     (forced-not :initform nil :initarg :force-not :accessor plan-forced-not)
     (total-action-count :initform 0 :accessor plan-total-action-count)
     (planned-action-count :initform 0 :accessor plan-planned-action-count)
     (planned-output-action-count :initform 0 :accessor plan-planned-output-action-count)
     (visited-actions :initform (make-hash-table :test 'equal) :accessor plan-visited-actions)
     (visiting-action-set :initform (make-hash-table :test 'equal) :accessor plan-visiting-action-set)
     (visiting-action-list :initform () :accessor plan-visiting-action-list))))


;;;; Planned action status
(with-upgradability ()
  (defgeneric plan-action-status (plan operation component)
    (:documentation "Returns the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defgeneric (setf plan-action-status) (new-status plan operation component)
    (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defclass planned-action-status (action-status)
    ((planned-p
      :initarg :planned-p :reader action-planned-p
      :documentation "a boolean, true iff the action was included in the plan.")
     (index
      :initarg :index :reader action-index
      :documentation "an integer, counting all traversed actions in traversal order."))
    (:documentation "Status of an action in a plan"))

  (defmethod print-object ((status planned-action-status) stream)
    (print-unreadable-object (status stream :type t :identity nil)
      (with-slots (stamp done-p planned-p index) status
        (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p :planned-p planned-p :index index))))

  (defmethod action-planned-p (action-status)
    (declare (ignorable action-status)) ; default method for non planned-action-status objects
    t)

  ;; TODO: eliminate NODE-FOR, use CONS.
  ;; Supposes cleaner protocol for operation initargs passed to MAKE-OPERATION.
  ;; However, see also component-operation-time and mark-operation-done
  (defun node-for (o c) (cons (type-of o) c))

  (defun action-already-done-p (plan operation component)
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
    new-status))


;;;; forcing
(with-upgradability ()
  (defgeneric action-forced-p (plan operation component))
  (defgeneric action-forced-not-p (plan operation component))

  (defun normalize-forced-systems (x system)
    (etypecase x
      ((member nil :all) x)
      (cons (list-to-hash-set (mapcar #'coerce-name x)))
      ((eql t) (when system (list-to-hash-set (list (coerce-name system)))))))

  (defun action-override-p (plan operation component override-accessor)
    (declare (ignorable operation))
    (let* ((override (funcall override-accessor plan)))
      (and override
           (if (typep override 'hash-table)
               (gethash (coerce-name (component-system (find-component () component))) override)
               t))))

  (defmethod action-forced-p (plan operation component)
    (and
     ;; Did the user ask us to re-perform the action?
     (action-override-p plan operation component 'plan-forced)
     ;; You really can't force a builtin system and :all doesn't apply to it,
     ;; except it it's the specifically the system currently being built.
     (not (let ((system (component-system component)))
            (and (builtin-system-p system)
                 (not (eq system (plan-system plan))))))))

  (defmethod action-forced-not-p (plan operation component)
    (and
     ;; Did the user ask us to not re-perform the action?
     (action-override-p plan operation component 'plan-forced-not)
     ;; Force takes precedence over force-not
     (not (action-forced-p plan operation component))))

  (defmethod action-forced-p ((plan null) operation component)
    (declare (ignorable plan operation component))
    nil)

  (defmethod action-forced-not-p ((plan null) operation component)
    (declare (ignorable plan operation component))
    nil))


;;;; action-valid-p
(with-upgradability ()
  (defgeneric action-valid-p (plan operation component)
    (:documentation "Is this action valid to include amongst dependencies?"))
  (defmethod action-valid-p (plan operation (c component))
    (declare (ignorable plan operation))
    (if-let (it (component-if-feature c)) (featurep it) t))
  (defmethod action-valid-p (plan (o null) c) (declare (ignorable plan o c)) nil)
  (defmethod action-valid-p (plan o (c null)) (declare (ignorable plan o c)) nil)
  (defmethod action-valid-p ((plan null) operation component)
    (declare (ignorable plan operation component))
    (and operation component t)))


;;;; Is the action needed in this image?
(with-upgradability ()
  (defgeneric needed-in-image-p (operation component)
    (:documentation "Is the action of OPERATION on COMPONENT needed in the current image to be meaningful,
    or could it just as well have been done in another Lisp image?"))

  (defmethod needed-in-image-p ((o operation) (c component))
    ;; We presume that actions that modify the filesystem don't need be run
    ;; in the current image if they have already been done in another,
    ;; and can be run in another process (e.g. a fork),
    ;; whereas those that don't are meant to side-effect the current image and can't.
    (not (output-files o c))))


;;;; Visiting dependencies of an action and computing action stamps
(with-upgradability ()
  (defun map-direct-dependencies (operation component fun)
    (loop* :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
           :unless (eq dep-o-spec 'feature) ;; avoid the "FEATURE" misfeature
           :do (loop :with dep-o = (find-operation operation dep-o-spec)
                     :for dep-c-spec :in dep-c-specs
                     :for dep-c = (resolve-dependency-spec component dep-c-spec)
                     :do (funcall fun dep-o dep-c))))

  (defun reduce-direct-dependencies (operation component combinator seed)
    (map-direct-dependencies
     operation component
     #'(lambda (dep-o dep-c)
         (setf seed (funcall combinator dep-o dep-c seed))))
    seed)

  (defun direct-dependencies (operation component)
    (reduce-direct-dependencies operation component #'acons nil))

  (defun visit-dependencies (plan operation component dependency-stamper &aux stamp)
    (map-direct-dependencies
     operation component
     #'(lambda (dep-o dep-c)
         (when (action-valid-p plan dep-o dep-c)
           (latest-stamp-f stamp (funcall dependency-stamper dep-o dep-c)))))
    stamp)

  (defmethod compute-action-stamp (plan (o operation) (c component) &key just-done)
    ;; In a distant future, get-file-stamp and component-operation-time
    ;; shall also be parametrized by the plan, or by a second model object.
    (let* ((stamp-lookup #'(lambda (o c)
                             (if-let (it (plan-action-status plan o c)) (action-stamp it) t)))
           (out-files (output-files o c))
           (in-files (input-files o c))
           ;; Three kinds of actions:
           (out-op (and out-files t)) ; those that create files on the filesystem
                                        ;(image-op (and in-files (null out-files))) ; those that load stuff into the image
                                        ;(null-op (and (null out-files) (null in-files))) ; dependency placeholders that do nothing
           ;; When was the thing last actually done? (Now, or ask.)
           (op-time (or just-done (component-operation-time o c)))
           ;; Accumulated timestamp from dependencies (or T if forced or out-of-date)
           (dep-stamp (visit-dependencies plan o c stamp-lookup))
           ;; Time stamps from the files at hand, and whether any is missing
           (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
           (in-stamps (mapcar #'get-file-stamp in-files))
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
              (action-description o c)
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
          (values t nil)))))


;;;; Generic support for plan-traversal
(with-upgradability ()
  (defgeneric plan-record-dependency (plan operation component))

  (defgeneric call-while-visiting-action (plan operation component function)
    (:documentation "Detect circular dependencies"))

  (defmethod initialize-instance :after ((plan plan-traversal)
                                         &key (force () fp) (force-not () fnp) system
                                         &allow-other-keys)
    (with-slots (forced forced-not) plan
      (when fp (setf forced (normalize-forced-systems force system)))
      (when fnp (setf forced-not (normalize-forced-systems force-not system)))))

  (defmethod (setf plan-action-status) (new-status (plan plan-traversal) (o operation) (c component))
    (setf (gethash (node-for o c) (plan-visited-actions plan)) new-status))

  (defmethod plan-action-status ((plan plan-traversal) (o operation) (c component))
    (or (and (action-forced-not-p plan o c) (plan-action-status nil o c))
        (values (gethash (node-for o c) (plan-visited-actions plan)))))

  (defmethod action-valid-p ((plan plan-traversal) (o operation) (s system))
    (and (not (action-forced-not-p plan o s)) (call-next-method)))

  (defmethod call-while-visiting-action ((plan plan-traversal) operation component fun)
    (with-accessors ((action-set plan-visiting-action-set)
                     (action-list plan-visiting-action-list)) plan
      (let ((action (cons operation component)))
        (when (gethash action action-set)
          (error 'circular-dependency :actions
                 (member action (reverse action-list) :test 'equal)))
        (setf (gethash action action-set) t)
        (push action action-list)
        (unwind-protect
             (funcall fun)
          (pop action-list)
          (setf (gethash action action-set) nil))))))


;;;; Actual traversal: traverse-action
(with-upgradability ()
  (define-condition circular-dependency (system-definition-error)
    ((actions :initarg :actions :reader circular-dependency-actions))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Circular dependency: ~3i~_~S~@:>")
                       (circular-dependency-actions c)))))

  (defmacro while-visiting-action ((p o c) &body body)
    `(call-while-visiting-action ,p ,o ,c #'(lambda () ,@body)))

  (defgeneric traverse-action (plan operation component needed-in-image-p))

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
                   (visit-dependencies plan operation component
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
            (visit-action eniip)))))))


;;;; Sequential plans (the default)
(with-upgradability ()
  (defclass sequential-plan (plan-traversal)
    ((actions-r :initform nil :accessor plan-actions-r)))

  (defgeneric plan-actions (plan))
  (defmethod plan-actions ((plan sequential-plan))
    (reverse (plan-actions-r plan)))

  (defmethod plan-record-dependency ((plan sequential-plan)
                                     (operation operation) (component component))
    (declare (ignorable plan operation component))
    (values))

  (defmethod (setf plan-action-status) :after
      (new-status (p sequential-plan) (o operation) (c component))
    (when (action-planned-p new-status)
      (push (cons o c) (plan-actions-r p)))))


;;;; high-level interface: traverse, perform-plan, plan-operates-on-p
(with-upgradability ()
  (defgeneric* (traverse) (operation component &key &allow-other-keys)
    (:documentation
     "Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))
  (define-convenience-action-methods traverse (operation component &key))

  (defgeneric perform-plan (plan &key))
  (defgeneric plan-operates-on-p (plan component))

  (defparameter *default-plan-class* 'sequential-plan)

  (defmethod traverse ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-instance
                       (or plan-class *default-plan-class*)
                       :system (component-system c) (remove-plist-key :plan-class keys))))
      (traverse-action plan o c t)
      (plan-actions plan)))

  (defmethod perform-plan :around (plan &key)
    (declare (ignorable plan))
    (let ((*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit () ;; backward-compatibility.
        (call-next-method))))   ;; Going forward, see deferred-warning support in lisp-build.

  (defmethod perform-plan ((steps list) &key)
    (loop* :for (op . component) :in steps :do
           (perform-with-restarts op component)))

  (defmethod plan-operates-on-p ((plan list) (component-path list))
    (find component-path (mapcar 'cdr plan)
          :test 'equal :key 'component-find-path)))


;;;; Incidental traversals 
(with-upgradability ()
  (defclass filtered-sequential-plan (sequential-plan)
    ((action-filter :initform t :initarg :action-filter :reader plan-action-filter)
     (component-type :initform t :initarg :component-type :reader plan-component-type)
     (keep-operation :initform t :initarg :keep-operation :reader plan-keep-operation)
     (keep-component :initform t :initarg :keep-component :reader plan-keep-component)))

  (defmethod initialize-instance :after ((plan filtered-sequential-plan)
                                         &key (force () fp) (force-not () fnp)
                                           other-systems)
    (declare (ignore force force-not))
    (with-slots (forced forced-not action-filter system) plan
      (unless fp (setf forced (normalize-forced-systems (if other-systems :all t) system)))
      (unless fnp (setf forced-not (normalize-forced-systems (if other-systems nil :all) system)))
      (setf action-filter (ensure-function action-filter))))

  (defmethod action-valid-p ((plan filtered-sequential-plan) o c)
    (and (funcall (plan-action-filter plan) o c)
         (typep c (plan-component-type plan))
         (call-next-method)))

  (defmethod traverse-actions (actions &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-instance (or plan-class 'filtered-sequential-plan) keys)))
      (loop* :for (o . c) :in actions :do
             (traverse-action plan o c t))
      (plan-actions plan)))

  (define-convenience-action-methods traverse-sub-actions (o c &key))
  (defmethod traverse-sub-actions ((operation operation) (component component) &rest keys &key &allow-other-keys)
    (apply 'traverse-actions (direct-dependencies operation component)
           :system (component-system component) keys))

  (defmethod plan-actions ((plan filtered-sequential-plan))
    (with-slots (keep-operation keep-component) plan
      (loop* :for (o . c) :in (call-next-method)
             :when (and (typep o keep-operation)
                        (typep c keep-component))
             :collect (cons o c))))

  (defmethod required-components (system &rest keys &key (goal-operation 'load-op) &allow-other-keys)
    (remove-duplicates
     (mapcar 'cdr (apply 'traverse-sub-actions goal-operation system keys))
     :from-end t)))

