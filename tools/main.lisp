;;;; Generic code to interface a Lisp script to the shell command-line.
(in-package :asdf-tools)

(defun re (arg)
  "Read-Eval function (the RE of REPL)
(the Print and Loop parts are not here)"
  (eval (read-from-string arg)))

(defun find-command (x &optional earlyp)
  "Find the function for an asdf-tools command by name"
  (block nil
    (flet ((try (x)
             (multiple-value-bind (sym foundp)
                 (find-symbol* (string-upcase x) :asdf-tools nil)
               (when (and sym (or earlyp (and foundp (fboundp sym))))
                 (return sym)))))
      (try (strcat "%" (string x))) ;; so that you may use load, t, etc., as targets
      (try x))))

(defun command-name (x &optional earlyp)
  (let ((c (find-command x earlyp)))
    (when c
      (let ((s (string-downcase c)))
        (if (eql (first-char s) #\%) (subseq s 1) s)))))

(defun short-function-description (x)
  "Short function description for x"
  (when (stringp x) (setf x (find-command x)))
  (if-let ((doc (and x (documentation x 'function))))
    (let* ((first-line (with-input (s doc) (read-line s)))
           (len (length first-line)))
      (if (>= len 50) (strcat (subseq first-line 0 49) "…") first-line))))

(defun public-commands ()
  ;;(loop :for x :being :the :external-symbols :of :asdf-tools
  ;; :when (and (eq x (find-command x)) (documentation x 'function)) :collect x)
  '(build-asdf doc website wc ;; build
    clean %push merge-master-into-release fix-local-git-tags fix-remote-git-tags ;; git
    git-all-committed-p
    bump-version bump ;; version
    test-load-systems test-clean-load test-basic %load install-asdf ;; test-basic
    %test %t test-scripts ;; test-scripts
    test-upgrade u extract-tagged-asdf extract-all-tagged-asdf extract ;; test-upgrade
    test-all-clean-load test-all-scripts test-all-no-upgrade test-all-upgrade ;; test-all
    test-all test-all-scripts-no-stop test-all-upgrade-no-stop
    test-all-no-upgrade-no-stop test-all-no-stop
    check-all-scripts-results check-all-upgrade-results check-all-results
    make-archive make-and-publish-archive publish-archive link-archive archive install ;; release
    debian-package publish-debian-package
    re help show-commands makefile-targets)) ;; main

(defun public-command-strings ()
  (sort (mapcar 'command-name (public-commands)) 'string<))

(defun show-commands ()
  "print the (sorted list of) names of all the public commands of asdf-tools."
  (format t "~{~A~^ ~}~%" (public-command-strings))
  (values))

(defun makefile-targets ()
  "print declaration for the public commands of asdf-tools as as many Makefile targets
so that, when included in the Makefile, they will enable shell completion
based on a list of targets"
  (let ((c (public-command-strings)))
    (format t ".PHONY: ~{~A~^ ~}~%~%~{~A~^ ~}: force
	./tools/asdf-tools env l='$l' L='$L' u='$u' U='$u' v='$v' s='$s' t='$t' $@~%" c c))
  (values))

(defun help (&optional x)
  "help about a command, or list of commands"
  (cond
    ((null x)
     (loop :for x :in (public-command-strings)
           :do (format t "~(~27A~)~@[  ~A~]~%"
                       x (short-function-description x)))
     (values))
    (t
     (let ((x (find-command x)))
       (when x
         (format t "~A~@[ ~A~]~%~@[~A~]~&"
                 (command-name x)
                 (or ()) ;; TODO: remember the arguments to deftestcmd, translate to v=, etc
                 (documentation x 'function))
         (values))))))

;;; Main entry point.
;;; NB: For access control, you could check that only exported symbols are used as entry points.
(defun main (args)
  (block nil
    (unless args
      (format t "No command provided~%")
      (return))
    (if-let ((fun (find-command (first args))))
      (let ((results
              (multiple-value-list (apply fun (rest args)))))
        (when results
          (format t "~&~{~S~%~}" results))
        (return (or (null results) (first results))))) ;; Trick: (values) counts as T, not NIL.
    (format t "Command ~A not found~%" (first args))
    (return)))


(defun entry-point ()
  (setf *lisp-interaction* nil)
  (uiop:with-fatal-condition-handler ()
    (initialize-environment)
    (main *command-line-arguments*)))
