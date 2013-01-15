;;;; -------------------------------------------------------------------------
;;;; run-program/ initially from xcvb-driver.

(asdf/package:define-package :asdf/run-program
  (:recycle :asdf/run-program :xcvb-driver)
  (:use :common-lisp :asdf/utility :asdf/pathname :asdf/stream :asdf/os)
  (:export
   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-token #:escape-command

   ;;; run-program/
   #:slurp-input-stream
   #:run-program/
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process
   ))
(in-package :asdf/run-program)

;;;; ----- Escaping strings for the shell -----

(defun* requires-escaping-p (token &key good-chars bad-chars)
  "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
  (some
   (cond
     ((and good-chars bad-chars)
      (error "only one of good-chars and bad-chars can be provided"))
     ((functionp good-chars)
      (complement good-chars))
     ((functionp bad-chars)
      bad-chars)
     ((and good-chars (typep good-chars 'sequence))
      (lambda (c) (not (find c good-chars))))
     ((and bad-chars (typep bad-chars 'sequence))
      (lambda (c) (find c bad-chars)))
     (t (error "requires-escaping-p: no good-char criterion")))
   token))

(defun* escape-token (token &key stream quote good-chars bad-chars escaper)
  "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
  (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
      (with-output (stream)
        (apply escaper token stream (when quote `(:quote ,quote))))
      (output-string token stream)))

(defun* escape-windows-token-within-double-quotes (x &optional s)
  "Escape a string token X within double-quotes
for use within a MS Windows command-line, outputing to S."
  (labels ((issue (c) (princ c s))
           (issue-backslash (n) (loop :repeat n :do (issue #\\))))
    (loop
      :initially (issue #\") :finally (issue #\")
      :with l = (length x) :with i = 0
      :for i+1 = (1+ i) :while (< i l) :do
      (case (char x i)
        ((#\") (issue-backslash 1) (issue #\") (setf i i+1))
        ((#\\)
         (let* ((j (and (< i+1 l) (position-if-not
                                   (lambda (c) (eql c #\\)) x :start i+1)))
                (n (- (or j l) i)))
           (cond
             ((null j)
              (issue-backslash (* 2 n)) (setf i l))
             ((and (< j l) (eql (char x j) #\"))
              (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
             (t
              (issue-backslash n) (setf i j)))))
        (otherwise
         (issue (char x i)) (setf i i+1))))))

(defun* escape-windows-token (token &optional s)
  "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
  (escape-token token :stream s :bad-chars #(#\space #\tab #\") :quote nil
                :escaper 'escape-windows-token-within-double-quotes))

(defun* escape-sh-token-within-double-quotes (x s &key (quote t))
  "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
  (when quote (princ #\" s))
  (loop :for c :across x :do
    (when (find c "$`\\\"") (princ #\\ s))
    (princ c s))
  (when quote (princ #\" s)))

(defun* easy-sh-character-p (x)
  (or (alphanumericp x) (find x "+-_.,%@:/")))

(defun* escape-sh-token (token &optional s)
  "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
  (escape-token token :stream s :quote #\" :good-chars
                #'easy-sh-character-p
                :escaper 'escape-sh-token-within-double-quotes))

(defun* escape-shell-token (token &optional s)
  (cond
    ((os-unix-p) (escape-sh-token token s))
    ((os-windows-p) (escape-windows-token token s))))

(defun* escape-command (command &optional s
                       (escaper 'escape-shell-token))
  "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
  (etypecase command
    (string (output-string command s))
    (list (with-output (s)
            (loop :for first = t :then nil :for token :in command :do
              (unless first (princ #\space s))
              (funcall escaper token s))))))

(defun* escape-windows-command (command &optional s)
  "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
  (escape-command command s 'escape-windows-token))

(defun* escape-sh-command (command &optional s)
  "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
  (escape-command command s 'escape-sh-token))

(defun* escape-shell-command (command &optional stream)
  "Escape a command for the current operating system's shell"
  (escape-command command stream 'escape-shell-token))


;;;; Slurping a stream, typically the output of another program

(defgeneric* slurp-input-stream (processor input-stream &key &allow-other-keys))

#-(or gcl<2.7 genera)
(defmethod slurp-input-stream ((function function) input-stream &key &allow-other-keys)
  (funcall function input-stream))

(defmethod slurp-input-stream ((list cons) input-stream &key &allow-other-keys)
  (apply (first list) (cons input-stream (rest list))))

#-(or gcl<2.7 genera)
(defmethod slurp-input-stream ((output-stream stream) input-stream
                               &key (element-type 'character) &allow-other-keys)
  (copy-stream-to-stream
   input-stream output-stream :element-type element-type))

(defmethod slurp-input-stream ((x (eql 'string)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-string stream))

(defmethod slurp-input-stream ((x (eql :string)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-string stream))

(defmethod slurp-input-stream ((x (eql :lines)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-lines stream))

(defmethod slurp-input-stream ((x (eql :form)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (read stream))

(defmethod slurp-input-stream ((x (eql :forms)) stream &key &allow-other-keys)
  (declare (ignorable x))
  (slurp-stream-forms stream))

(defmethod slurp-input-stream (x stream &key (element-type 'character) &allow-other-keys)
  (declare (ignorable stream element-type))
  (cond
    #+(or gcl<2.7 genera)
    ((functionp x) (funcall x stream))
    #+(or gcl<2.7 genera)
    ((output-stream-p x) (copy-stream-to-stream stream x :element-type element-type))
    (t
     (error "Invalid ~S destination ~S" 'slurp-input-stream x))))


;;;; ----- Running an external program -----
;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...

(define-condition subprocess-error (error)
  ((code :initform nil :initarg :code :reader subprocess-error-code)
   (command :initform nil :initarg :command :reader subprocess-error-command)
   (process :initform nil :initarg :process :reader subprocess-error-process))
  (:report (lambda (condition stream)
             (format stream "Subprocess~@[ ~S~]~@[ run with command ~S~] exited with error~@[ code ~D~]"
                     (subprocess-error-process condition)
                     (subprocess-error-command condition)
                     (subprocess-error-code condition)))))

(defun* run-program/ (command
                     &key output ignore-error-status force-shell
                     (element-type *default-stream-element-type*)
                     (external-format :default)
                     &allow-other-keys)
  "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows);
have its output processed by the OUTPUT processor function
as per SLURP-INPUT-STREAM,
or merely output to the inherited standard output if it's NIL.
Always call a shell (rather than directly execute the command)
if FORCE-SHELL is specified.
Issue an error if the process wasn't successful unless IGNORE-ERROR-STATUS
is specified.
Return the exit status code of the process that was called.
Use ELEMENT-TYPE and EXTERNAL-FORMAT for the stream passed to the OUTPUT processor."
  (declare (ignorable ignore-error-status element-type external-format))
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl lispworks mcl sbcl scl xcl)
  (error "RUN-PROGRAM/ not implemented for this Lisp")
  (labels (#+(or allegro clisp clozure cmu ecl (and lispworks os-unix) sbcl scl)
           (run-program (command &key pipe interactive)
             "runs the specified command (a list of program and arguments).
              If using a pipe, returns two values: process and stream
              If not using a pipe, returns one values: the process result;
              also, inherits the output stream."
             ;; NB: these implementations have unix vs windows set at compile-time.
	     (assert (not (and pipe interactive)))
             (let* ((wait (not pipe))
                    #-(and clisp os-windows)
                    (command
                     (etypecase command
                       #+os-unix (string `("/bin/sh" "-c" ,command))
                       #+os-unix (list command)
                       #+os-windows
                       (string
                        ;; NB: We do NOT add cmd /c here. You might want to.
                        #+allegro command
			;; On ClozureCL for Windows, we assume you are using
			;; r15398 or later in 1.9 or later,
			;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
			#+clozure (cons "cmd" (strcat "/c " command))
                        ;; NB: On other Windows implementations, this is utterly bogus
                        ;; except in the most trivial cases where no quoting is needed.
                        ;; Use at your own risk.
                        #-(or allegro clozure) (list "cmd" "/c" command))
                       #+os-windows
                       (list
                        #+(or allegro clozure) (escape-windows-command command)
                        #-(or allegro clozure) command)))
                    #+(and clozure os-windows) (command (list command))
                    (process*
                     (multiple-value-list
                      #+allegro
                      (excl:run-shell-command
                       #+os-unix (coerce (cons (first command) command) 'vector)
                       #+os-windows command
                       :input interactive :output (or (and pipe :stream) interactive) :wait wait
                       #+os-windows :show-window #+os-windows (and pipe :hide))
                      #+clisp
                      (flet ((run (f &rest args)
                               (apply f `(,@args :input ,(when interactive :terminal) :wait ,wait :output
                                          ,(if pipe :stream :terminal)))))
                        (etypecase command
                          #+os-windows (run 'ext:run-shell-command command)
                          (list (run 'ext:run-program (car command)
                                     :arguments (cdr command)))))
                      #+lispworks
                      (system:run-shell-command
                       (cons "/usr/bin/env" command) ; lispworks wants a full path.
                       :input interactive :output (or (and pipe :stream) interactive)
                       :wait wait :save-exit-status (and pipe t))
                      #+(or clozure cmu ecl sbcl scl)
                      (#+(or cmu ecl scl) ext:run-program
                       #+clozure ccl:run-program
                       #+sbcl sb-ext:run-program
                       (car command) (cdr command)
                       :input interactive :wait wait
                       :output (if pipe :stream t)
                       . #.(append
                            #+(or clozure cmu ecl sbcl scl) '(:error t)
                            ;; note: :external-format requires a recent SBCL
                            #+sbcl '(:search t :external-format external-format)))))
                    (process
                     #+(or allegro lispworks) (if pipe (third process*) (first process*))
                     #+ecl (third process*)
                     #-(or allegro lispworks ecl) (first process*))
                    (stream
                     (when pipe
                       #+(or allegro lispworks ecl) (first process*)
                       #+clisp (first process*)
                       #+clozure (ccl::external-process-output process)
                       #+(or cmu scl) (ext:process-output process)
                       #+sbcl (sb-ext:process-output process))))
               (values process stream)))
           #+(or allegro clisp clozure cmu ecl (and lispworks os-unix) sbcl scl)
           (process-result (process pipe)
             (declare (ignorable pipe))
             ;; 1- wait
             #+(and clozure os-unix) (ccl::external-process-wait process)
             #+(or cmu scl) (ext:process-wait process)
             #+(and ecl os-unix) (ext:external-process-wait process)
             #+sbcl (sb-ext:process-wait process)
             ;; 2- extract result
             #+allegro (if pipe (sys:reap-os-subprocess :pid process :wait t) process)
             #+clisp process
             #+clozure (nth-value 1 (ccl:external-process-status process))
             #+(or cmu scl) (ext:process-exit-code process)
             #+ecl (nth-value 1 (ext:external-process-status process))
             #+lispworks (if pipe (system:pid-exit-status process :wait t) process)
             #+sbcl (sb-ext:process-exit-code process))
           (check-result (exit-code process)
             #+clisp
             (setf exit-code
                   (typecase exit-code (integer exit-code) (null 0) (t -1)))
             (unless (or ignore-error-status
                         (equal exit-code 0))
               (error 'subprocess-error :command command :code exit-code :process process))
	     exit-code)
           (use-run-program ()
             #-(or abcl cormanlisp gcl (and lispworks os-windows) mcl mkcl xcl)
             (let* ((interactive (eq output :interactive))
		    (pipe (and output (not interactive))))
               (multiple-value-bind (process stream)
                   (run-program command :pipe pipe :interactive interactive)
                 (if (and output (not interactive))
                     (unwind-protect
                          (slurp-input-stream output stream)
                       (when stream (close stream))
                       (check-result (process-result process pipe) process))
                     (unwind-protect
                          (check-result
                           #+(or allegro lispworks) ; when not capturing, returns the exit code!
                           process
                           #-(or allegro lispworks) (process-result process pipe)
                           process))))))
           (system-command (command)
             (etypecase command
               (string (if (os-windows-p) (format nil "cmd /c ~A" command) command))
               (list (escape-shell-command
                      (if (os-unix-p) (cons "exec" command) command)))))
           (redirected-system-command (command out)
             (format nil (if (os-unix-p) "exec > ~*~A ; ~2:*~A" "~A > ~A")
                     (system-command command) (native-namestring out)))
           (system (command &key interactive)
             (declare (ignorable interactive))
             #+(or abcl xcl) (ext:run-shell-command command)
             #+allegro
             (excl:run-shell-command command :input interactive :output interactive :wait t)
             #+(or clisp clozure cmu (and lispworks os-unix) sbcl scl)
             (process-result (run-program command :pipe nil :interactive interactive) nil)
             #+ecl (ext:system command)
             #+cormanlisp (win32:system command)
             #+gcl (lisp:system command)
             #+(and lispworks os-windows)
             (system:call-system-showing-output
              command :show-cmd interactive :prefix "" :output-stream nil)
             #+mcl (ccl::with-cstrs ((%command command)) (_system %command))
             #+mkcl (nth-value 2
                               (mkcl:run-program #+windows command #+windows ()
                                                 #-windows "/bin/sh" (list "-c" command)
                                                 :input nil :output nil)))
           (call-system (command-string &key interactive)
             (check-result (system command-string :interactive interactive) nil))
           (use-system ()
	     (let ((interactive (eq output :interactive)))
	       (if (and output (not interactive))
		   (with-temporary-file (:pathname tmp :direction :output)
		     (call-system (redirected-system-command command tmp))
		     (with-open-file (stream tmp
					     :direction :input
					     :if-does-not-exist :error
					     :element-type element-type
                                             #-gcl<2.7 :external-format #-gcl<2.7 external-format)
		       (slurp-input-stream output stream)))
		   (call-system (system-command command) :interactive interactive)))))
    (if (and (not force-shell)
             #+(or clisp ecl) ignore-error-status
             #+(or abcl cormanlisp gcl (and lispworks os-windows) mcl mkcl xcl) nil)
        (use-run-program)
        (use-system))))

