;;;; Generic code to interface a Lisp script to the shell command-line.

(in-package :asdf-tools)

(defun re (arg)
  ;;; Read-Eval function (the RE of REPL; the Print and Loop parts are not here)
  (eval (read-from-string arg)))

(defun find-command (x)
  "Find the function for an asdf-builder command by name"
  (block nil
    (flet ((try (x)
             (multiple-value-bind (sym foundp)
                 (find-symbol* (string-upcase x) :asdf-tools nil)
               (when (and sym (eq foundp :internal) (fboundp sym))
                 (return (fdefinition sym))))))
      (try (strcat "%" x)) ;; so that you may use load, t, etc., as targets
      (try x))))

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
        (return (first results))))
    (format t "Command ~A not found~%" (first args))
    (return)))


(defun entry-point ()
  (setf *lisp-interaction* nil)
  (uiop:with-fatal-condition-handler ()
    (initialize-environment)
    (main *command-line-arguments*)))
