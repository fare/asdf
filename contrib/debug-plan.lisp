(in-package :asdf/plan)

(uiop:uiop-debug)

(progn
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
    ;; yet a NIL done-in-image-p flag.
    (nest
     (block ())
     (let ((dep-stamp ; collect timestamp from dependencies (or T if forced or out-of-date)
             (reduce-direct-dependencies
              plan o c
              #'(lambda (o c stamp)
                  (if-let (it (plan-action-status plan o c))
                    (latest-stamp stamp (action-stamp it))
                    t))
              nil)))
       ;; out-of-date dependency: don't bother expensively querying the filesystem
       (when (and (eq dep-stamp t) (not just-done))
         (let ((outdated-deps
                 (reduce-direct-dependencies
                  plan o c
                  #'(lambda (o c deps)
                      (if (if-let (it (plan-action-status plan o c))
                            (not (eq t (action-stamp it))))
                          deps
                          (cons (cons o c) deps)))
                  nil)))
           (DBG "compute-action-stamp: forced by out of date dependency"
                (cons o c)
                outdated-deps))
         (return (values t nil))))
     ;; collect timestamps from inputs, and exit early if any is missing
     (let* ((in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (stamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done))
         (DBG "compute-action-stamp: missing inputs"
              (cons o c)
              missing-in)
         (return (values t nil))))
     ;; collect timestamps from outputs, and exit early if any is missing
     (let* ((out-files (remove-if 'null (output-files o c)))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (stamps-earliest out-stamps)))
       (when (and missing-out (not just-done))
         (DBG "compute-action-stamp: missing outputs"
              (cons o c)
              missing-out)
         (return (values t nil))))
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
               missing-out (length missing-out)))
       (let ((already-done-p (and all-present up-to-date-p (operation-done-p o c) (not (action-forced-p plan o c))))
             (matching-stamp-p (and op-time (eql op-time done-stamp))))
         (DBG "compute-action-stamp"
            (cons o c)
            in-files
            in-stamps
            dep-stamp
            out-files
            out-stamps
            latest-in
            earliest-out
            up-to-date-p
            op-time
            done-stamp
            ;; (eql op-time done-stamp)
            all-present
            already-done-p
            just-done
            out-op
            matching-stamp-p)))
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
