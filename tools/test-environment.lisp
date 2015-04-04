(in-package :asdf-tools)

;;; Testing

(defparameter *test-lisps* :default
  "The list of lisp implementations to use for tests")

(defparameter *upgrade-test-lisps* :default
  "The list of lisp implementations to use for upgrade tests")

(defparameter *upgrade-test-tags* :default)

(defparameter *test-systems* nil)

(defparameter *test-scripts* :default)

(defparameter *environment-variable-table* nil)

(defparameter *environment-variable-specs*
  '((*test-lisps* ensure-list-of-keywords :default
     "ASDF_TEST_LISPS" "l")
    (*upgrade-test-lisps* ensure-list-of-keywords :default
     "ASDF_UPGRADE_TEST_LISPS" "L")
    (*test-systems* ensure-list-of-keywords ()
     "ASDF_TEST_SYSTEMS" "s")
    (*test-scripts* ensure-list-of-strings :default
     "ASDF_TESTS" "t")
    (*upgrade-test-tags* ensure-list-of-strings :default
     "ASDF_UPGRADE_TEST_TAGS" "u")
    (*upgrade-test-methods* ensure-list-of-test-methods :default
     "ASDF_UPGRADE_TEST_METHODS" "U")
    (*new-version* string :default
     "=NEW_ASDF_VERSION" "v")))

(defun ensure-list-of-strings (x)
  (remove nil
          (etypecase x
            (string (uiop:split-string x :separator " "))
            (list x))))

(defun ensure-keyword (x)
  (etypecase x
    ((or null keyword) x)
    ((or string symbol) (intern (string-upcase x) :keyword))))

(defun ensure-list-of-keywords (x)
  (mapcar 'ensure-keyword (ensure-list-of-strings x)))

(defun ensure-test-method (x)
  (safe-read-from-string (strcat "(" (substitute #\space #\: x) ")") :package :keyword)) ; UGLY!

(defun ensure-list-of-test-methods (x)
  (mapcar 'ensure-test-method (ensure-list-of-strings x)))

(defun initialize-environment ()
  (let ((h (make-hash-table :test 'equal)))
    (setf *environment-variable-table* h)
    (loop :for (variable transformer defaults envvar short) :in *environment-variable-specs*
          :for x = (list variable transformer)
          :do (setf (symbol-value variable)
                    (if-let (x (getenvp envvar))
                      (funcall transformer x)
                      (eval defaults)))
              (setf (gethash envvar h) x)
              (setf (gethash short h) x))))

(defun display-environment ()
  (format t "Environment for ASDF tools:~%")
  (let ((*package* (find-package :asdf-tools)))
    (loop :for variable-name :in (mapcar 'first *environment-variable-specs*)
          :do (format t "~T~S = ~S~%"
                      variable-name (symbol-value variable-name)))))

(defun test-definition (def)
  (block ()
    (match def
      ((ppcre "^([^=]+)=(.*)$" var val)
       (if-let (x (gethash var *environment-variable-table*))
         (match x
           ((list* sym transformer _)
            (unless (emptyp val)
              (setf (symbol-value sym) (funcall transformer val)))
            (return t)))
         (error "Unknown variable ~A" var))))
    nil))

(defun show-environment ()
  (loop :for (v) :in *environment-variable-specs* :do
    (format t "~A = ~S~%" v (symbol-value v)))
  (values))

(defun env (&rest env)
  (loop :for (first . rest) :on env
        :unless (test-definition first)
          :return (if-let (c (find-command first))
                    (apply c rest)
                    (progn (format t "Command ~A not found~%" first) nil))
        :finally (progn (format t "No command provided~%") (return))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun decl-or-docstring-p (form)
    (or (stringp form)
        (and (consp form) (eq 'declare (car form)))))

  (defun decl-and-body (decl-and-body)
    (let ((p (position-if-not 'decl-or-docstring-p decl-and-body)))
      (values (subseq decl-and-body 0 p)
              (nthcdr p decl-and-body)))))

(defmacro deftestcmd (name args &rest decl-and-body)
  (loop :with argmap =
        '((lisp ((lisp *test-lisps*))
           (setf lisp (get-lisp lisp)))
          (lisps ((lisps *test-lisps*))
           (setf lisps (get-lisps lisps)))
          (systems ((systems *test-systems*)))
          (test-scripts ((test-scripts *test-scripts*))
           (setf test-scripts (get-test-scripts test-scripts)))
          (upgrade-tags ((upgrade-tags *upgrade-test-tags*))
           (setf upgrade-tags (get-upgrade-tags upgrade-tags)))
          (upgrade-methods ((upgrade-methods *upgrade-test-methods*))
           (setf upgrade-methods (get-upgrade-methods upgrade-methods)))
          (new-version (new-version)
           (setf new-version (or new-version (next-version (version-from-file))))))
        :for arg :in args
        :for (found larg init) = (assoc arg argmap)
        :append (if found larg (list arg)) :into largs
        :append (when found (list init)) :into inits
        :finally
           (multiple-value-bind (decl body) (decl-and-body decl-and-body)
             (return
               `(defun ,name (&optional ,@largs)
                  ,@decl
                  ,@inits
                  ,@body)))))

(defun all-pass (&rest tests)
  (every 'identity tests))

(defmacro defalias (name real)
  `(defun ,name (&rest args)
     ,(format nil "alias for command ~A" (command-name real t))
     (apply ',real args)))

(deftestcmd interactive-command (lisp)
  (let* ((command (lisp-invocation-arglist :implementation-type lisp :debugger t)))
    (cons "rlwrap" command)))

(defparameter *default-test-lisps*
  '(:ccl :clisp :sbcl :ecl :ecl_bytecodes :cmucl :abcl :scl :allegro
    :lispworks :allegromodern :clasp :gcl :xcl :mkcl)
  ;; NOT SUPPORTED BY OUR AUTOMATED TESTS:
  ;; :cormancl :genera :lispworks-personal-edition :mcl
  ;; Also, grep for #+/#- features in the test/ directory
  ;; to see plenty of disabled tests on some platforms
  "Default Lisp implementations for tests")

(defun get-lisps (&optional (lisps *test-lisps*))
  (if (eq lisps :default) *default-test-lisps* (ensure-list-of-keywords lisps)))

(defun get-lisp (&optional (lisp *test-lisps*))
  (if (and (keywordp lisp) (not (eq lisp :default))) lisp (first (get-lisps lisp))))

(defun date-string (&optional (date (get-universal-time)))
  (multiple-value-bind (second minute hour date month year weekday daylight-savings-p timezone)
      (decode-universal-time date)
    (declare (ignore second minute hour weekday daylight-savings-p timezone))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))

(deftestcmd newlogfile ((kind "log") lisp)
  (let ((log (pn (format nil "build/results/~(~A-~A~).text" lisp kind))))
    (ensure-directories-exist log)
    (if-let (date (safe-file-write-date log))
      (rename-file-overwriting-target
       log (add-pathname-suffix log (strcat "-" (date-string date)))))
    (with-output-file (s log) s) ;; create the file
    ;;(format t "Logging results to ~A" log)
    log))

(defun log! (log fmt &rest args)
  (let ((msg (apply 'format nil fmt args)))
    (format t "~&~A~&" msg)
    (when log
      (with-output-file (s log :if-exists :append :if-does-not-exist :create)
        ;; re-open every time because we're interleaved with inferior process writing to the log,
        ;; and on e.g. Windows there might be a locking conflict if we keep it open.
        (format s "~&~A~&" msg))))
  (values))

;; TODO: When composing a form to evaluate in the test lisp implementation,
;; our shell script went through great lengths to avoid a double-quote #\" in the command line,
;; the quoting of which many Windows implementations get wrong.
;; While we're at it, we also avoid spaces and backslashes.
;; We haven't tested our new Lisp implementation of the test infrastructure on Windows, though.

(defun run-test-lisp (activity forms &key (output t) log lisp debugger)
  ;; Activity is of the form "compiling ASDF", "running this test", etc.
  (format t "~&Now ~A...~@[ (log in ~A)~]~%" activity log)
  (let* ((eval (compose-eval-string forms))
         (command (lisp-invocation-arglist :implementation-type (get-lisp lisp)
                                           :eval eval :debugger debugger))
         (interactive (if (eq output :interactive) :interactive nil))
         (output (if (eq output t) *standard-output* output))
         (output (if (eq output *stdout*) :interactive output)))
    (log! log "~A" (print-process-spec command nil))
    (multiple-value-bind (okp out err code)
        (run* `((>& 2 1) ,@(when interactive '(rlwrap)) ,@command
                ,@(when log `((>> ,log)))) ;; unhappily, | tee -a log eats error codes :-(
              :input interactive :output output :error-output (or interactive :output) :on-error nil)
      (unless interactive
        (log! log (if okp
                      "SUCCEEDED at ~A."
                      "FAILED at ~A.
You can retry ~A with:
    ~A
or more interactively, start with:
    ~A~%(rlwrap is optional; don't use it when in emacs; skip if not installed.)
then copy/paste:
~@<    ~@;~A~@:>~&")
              activity activity
              (print-process-spec command nil)
              (print-process-spec (interactive-command) nil)
              (compose-copy-paste-string forms)))
    (values okp out err code))))
