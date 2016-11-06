;;;; -------------------------------------------------------------------------
;;;; run-program initially from xcvb-driver.

(uiop/package:define-package :uiop/run-program
  (:nicknames :asdf/run-program) ; OBSOLETE. Used by cl-sane, printv.
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream)
  (:export
   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-token #:escape-command

   ;;; run-program
   #:slurp-input-stream #:vomit-output-stream
   #:close-streams #:launch-program #:process-alive-p #:run-program
   #:terminate-process #:wait-process
   #:process-info-error-output #:process-info-input #:process-info-output
   #:process-info-pid
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process
   ))
(in-package :uiop/run-program)

;;;; ----- Escaping strings for the shell -----

(with-upgradability ()
  (defun requires-escaping-p (token &key good-chars bad-chars)
    "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
    (some
     (cond
       ((and good-chars bad-chars)
        (error "only one of good-chars and bad-chars can be provided"))
       ((typep good-chars 'function)
        (complement good-chars))
       ((typep bad-chars 'function)
        bad-chars)
       ((and good-chars (typep good-chars 'sequence))
        #'(lambda (c) (not (find c good-chars))))
       ((and bad-chars (typep bad-chars 'sequence))
        #'(lambda (c) (find c bad-chars)))
       (t (error "requires-escaping-p: no good-char criterion")))
     token))

  (defun escape-token (token &key stream quote good-chars bad-chars escaper)
    "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
    (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
        (with-output (stream)
          (apply escaper token stream (when quote `(:quote ,quote))))
        (output-string token stream)))

  (defun escape-windows-token-within-double-quotes (x &optional s)
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
                                       #'(lambda (c) (eql c #\\)) x :start i+1)))
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

  (defun easy-windows-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,@:/=")))

  (defun escape-windows-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
    (escape-token token :stream s :good-chars #'easy-windows-character-p :quote nil
                        :escaper 'escape-windows-token-within-double-quotes))

  (defun escape-sh-token-within-double-quotes (x s &key (quote t))
    "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
    (when quote (princ #\" s))
    (loop :for c :across x :do
      (when (find c "$`\\\"") (princ #\\ s))
      (princ c s))
    (when quote (princ #\" s)))

  (defun easy-sh-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,%@:/=")))

  (defun escape-sh-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
    (escape-token token :stream s :quote #\" :good-chars #'easy-sh-character-p
                        :escaper 'escape-sh-token-within-double-quotes))

  (defun escape-shell-token (token &optional s)
    "Escape a token for the current operating system shell"
    (os-cond
      ((os-unix-p) (escape-sh-token token s))
      ((os-windows-p) (escape-windows-token token s))))

  (defun escape-command (command &optional s
                                  (escaper 'escape-shell-token))
    "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
    (etypecase command
      (string (output-string command s))
      (list (with-output (s)
              (loop :for first = t :then nil :for token :in command :do
                (unless first (princ #\space s))
                (funcall escaper token s))))))

  (defun escape-windows-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
    (escape-command command s 'escape-windows-token))

  (defun escape-sh-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
    (escape-command command s 'escape-sh-token))

  (defun escape-shell-command (command &optional stream)
    "Escape a command for the current operating system's shell"
    (escape-command command stream 'escape-shell-token)))


;;;; Slurping a stream, typically the output of another program
(with-upgradability ()
  (defun call-stream-processor (fun processor stream)
    "Given FUN (typically SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM,
a PROCESSOR specification which is either an atom or a list specifying
a processor an keyword arguments, call the specified processor with
the given STREAM as input"
    (if (consp processor)
        (apply fun (first processor) stream (rest processor))
        (funcall fun processor stream)))

  (defgeneric slurp-input-stream (processor input-stream &key)
    (:documentation
     "SLURP-INPUT-STREAM is a generic function with two positional arguments
PROCESSOR and INPUT-STREAM and additional keyword arguments, that consumes (slurps)
the contents of the INPUT-STREAM and processes them according to a method
specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the INPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.  It will be applied to a cons of the
  INPUT-STREAM and the rest of the list.  That is (x . y) will be treated as
    \(APPLY x <stream> y\)
* if PROCESSOR is an output-stream, the contents of INPUT-STREAM is copied to the output-stream,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is the symbol CL:STRING or the keyword :STRING, then the contents of INPUT-STREAM
  are returned as a string, as per SLURP-STREAM-STRING.
* if PROCESSOR is the keyword :LINES then the INPUT-STREAM will be handled by SLURP-STREAM-LINES.
* if PROCESSOR is the keyword :LINE then the INPUT-STREAM will be handled by SLURP-STREAM-LINE.
* if PROCESSOR is the keyword :FORMS then the INPUT-STREAM will be handled by SLURP-STREAM-FORMS.
* if PROCESSOR is the keyword :FORM then the INPUT-STREAM will be handled by SLURP-STREAM-FORM.
* if PROCESSOR is T, it is treated the same as *standard-output*. If it is NIL, NIL is returned.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod slurp-input-stream ((function function) input-stream &key)
    (funcall function input-stream))

  (defmethod slurp-input-stream ((list cons) input-stream &key)
    (apply (first list) input-stream (rest list)))

  #-genera
  (defmethod slurp-input-stream ((output-stream stream) input-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod slurp-input-stream ((x (eql 'string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :lines)) stream &key count)
    (slurp-stream-lines stream :count count))

  (defmethod slurp-input-stream ((x (eql :line)) stream &key (at 0))
    (slurp-stream-line stream :at at))

  (defmethod slurp-input-stream ((x (eql :forms)) stream &key count)
    (slurp-stream-forms stream :count count))

  (defmethod slurp-input-stream ((x (eql :form)) stream &key (at 0))
    (slurp-stream-form stream :at at))

  (defmethod slurp-input-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'slurp-input-stream *standard-output* stream keys))

  (defmethod slurp-input-stream ((x null) (stream t) &key)
    nil)

  (defmethod slurp-input-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod slurp-input-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((output-stream-p x)
       (copy-stream-to-stream
        stream x
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (error "Invalid ~S destination ~S" 'slurp-input-stream x)))))


(with-upgradability ()
  (defgeneric vomit-output-stream (processor output-stream &key)
    (:documentation
     "VOMIT-OUTPUT-STREAM is a generic function with two positional arguments
PROCESSOR and OUTPUT-STREAM and additional keyword arguments, that produces (vomits)
some content onto the OUTPUT-STREAM, according to a method specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the OUTPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.
  It will be applied to a cons of the OUTPUT-STREAM and the rest of the list.
  That is (x . y) will be treated as \(APPLY x <stream> y\)
* if PROCESSOR is an input-stream, its contents will be copied the OUTPUT-STREAM,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is a string, its contents will be printed to the OUTPUT-STREAM.
* if PROCESSOR is T, it is treated the same as *standard-input*. If it is NIL, nothing is done.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod vomit-output-stream ((function function) output-stream &key)
    (funcall function output-stream))

  (defmethod vomit-output-stream ((list cons) output-stream &key)
    (apply (first list) output-stream (rest list)))

  #-genera
  (defmethod vomit-output-stream ((input-stream stream) output-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod vomit-output-stream ((x string) stream &key fresh-line terpri)
    (princ x stream)
    (when fresh-line (fresh-line stream))
    (when terpri (terpri stream))
    (values))

  (defmethod vomit-output-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'vomit-output-stream *standard-input* stream keys))

  (defmethod vomit-output-stream ((x null) (stream t) &key)
    (values))

  (defmethod vomit-output-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod vomit-output-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((input-stream-p x)
       (copy-stream-to-stream
        x stream
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (error "Invalid ~S source ~S" 'vomit-output-stream x)))))


;;;; ----- Running an external program -----
;;; Simple variant of run-program with no input, and capturing output
;;; On some implementations, may output to a temporary file...
(with-upgradability ()
  (define-condition subprocess-error (error)
    ((code :initform nil :initarg :code :reader subprocess-error-code)
     (command :initform nil :initarg :command :reader subprocess-error-command)
     (process :initform nil :initarg :process :reader subprocess-error-process))
    (:report (lambda (condition stream)
               (format stream "Subprocess ~@[~S~% ~]~@[with command ~S~% ~]exited with error~@[ code ~D~]"
                       (subprocess-error-process condition)
                       (subprocess-error-command condition)
                       (subprocess-error-code condition)))))

  ;;; find CMD.exe on windows
  (defun %cmd-shell-pathname ()
    (os-cond
     ((os-windows-p)
      (strcat (native-namestring (getenv-absolute-directory "WINDIR"))
              "System32\\cmd.exe"))
     (t
      (error "CMD.EXE is not the command shell for this OS."))))

  ;;; Internal helpers for run-program
  (defun %normalize-command (command)
    "Given a COMMAND as a list or string, transform it in a format suitable
for the implementation's underlying run-program function"
    (etypecase command
      #+os-unix (string `("/bin/sh" "-c" ,command))
      #+os-unix (list command)
      #+os-windows
      (string
       ;; NB: We do NOT add cmd /c here. You might want to.
       #+(or allegro clisp) command
       ;; On ClozureCL for Windows, we assume you are using
       ;; r15398 or later in 1.9 or later,
       ;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
       #+clozure (cons "cmd" (strcat "/c " command))
       #+mkcl (list "cmd" "/c" command)
       #+sbcl (list (%cmd-shell-pathname) "/c" command)
       ;; NB: On other Windows implementations, this is utterly bogus
       ;; except in the most trivial cases where no quoting is needed.
       ;; Use at your own risk.
       #-(or allegro clisp clozure mkcl sbcl) (list "cmd" "/c" command))
      #+os-windows
      (list
       #+allegro (escape-windows-command command)
       #-allegro command)))

  (defun %active-io-specifier-p (specifier)
    "Determines whether a run-program I/O specifier requires Lisp-side processing
via SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM (return T),
or whether it's already taken care of by the implementation's underlying run-program."
    (not (typep specifier '(or null string pathname (member :interactive :output)
                            #+(or cmucl (and sbcl os-unix) scl) (or stream (eql t))
                            #+lispworks file-stream))))

  (defun %normalize-io-specifier (specifier &optional role)
    "Normalizes a portable I/O specifier for LAUNCH-PROGRAM into an implementation-dependent
argument to pass to the internal RUN-PROGRAM"
    (declare (ignorable role))
    (typecase specifier
      (null (or #+(or allegro lispworks) (null-device-pathname)))
      (string (parse-native-namestring specifier))
      (pathname specifier)
      (stream specifier)
      ((eql :stream) :stream)
      ((eql :interactive)
       #+allegro nil
       #+clisp :terminal
       #+(or abcl clozure cmucl ecl mkcl sbcl scl) t
       #-(or abcl clozure cmucl ecl mkcl sbcl scl allegro clisp)
       (not-implemented-error :interactive-output
                              "On this lisp implementation, cannot interpret ~a value of ~a"
                              specifier role))
      ((eql :output)
       (if (eq role :error-output)
          (or
             #+(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)  :output
             (not-implemented-error :error-output-redirect
                                    "Can't send ~a to ~a on this lisp implementation."
                                    role specifier))
          (parameter-error "~S IO specifier invalid for ~S" specifier role)))
      (otherwise
       (parameter-error "Incorrect I/O specifier ~S for ~S"
                        specifier role))))

  (defun %interactivep (input output error-output)
    (member :interactive (list input output error-output)))

  (defun %signal-to-exit-code (signum)
    (+ 128 signum))

  #+mkcl
  (defun %mkcl-signal-to-number (signal)
    (require :mk-unix)
    (symbol-value (find-symbol signal :mk-unix)))

  (defclass process-info ()
    ((process :initform nil)
     (input-stream :initform nil)
     (output-stream :initform nil)
     (bidir-stream :initform nil)
     (error-output-stream :initform nil)
     ;; For backward-compatibility, to maintain the property (zerop
     ;; exit-code) <-> success, an exit in response to a signal is
     ;; encoded as 128+signum.
     (exit-code :initform nil)
     ;; If the platform allows it, distinguish exiting with a code
     ;; >128 from exiting in response to a signal by setting this code
     (signal-code :initform nil)))

;;;---------------------------------------------------------------------------
;;; The following two helper functions take care of handling the IF-EXISTS and
;;; IF-DOES-NOT-EXIST arguments for RUN-PROGRAM. In particular, they process the
;;; :ERROR, :APPEND, and :SUPERSEDE arguments *here*, allowing the master
;;; function to treat input and output files unconditionally for reading and
;;; writing.
;;;---------------------------------------------------------------------------

  (defun %handle-if-exists (file if-exists)
    (when (or (stringp file) (pathnamep file))
      (ecase if-exists
        ((:append :supersede :error)
         (with-open-file (dummy file :direction :output :if-exists if-exists)
           (declare (ignorable dummy)))))))

  (defun %handle-if-does-not-exist (file if-does-not-exist)
    (when (or (stringp file) (pathnamep file))
      (ecase if-does-not-exist
        ((:create :error)
         (with-open-file (dummy file :direction :probe
                                :if-does-not-exist if-does-not-exist)
           (declare (ignorable dummy)))))))

  (defun launch-program (command &rest keys
                         &key
                           input (if-input-does-not-exist :error)
                           output (if-output-exists :supersede)
                           error-output (if-error-output-exists :supersede)
                           (element-type #-clozure *default-stream-element-type*
                                         #+clozure 'character)
                           (external-format *utf-8-external-format*)
                           directory
                           #+allegro separate-streams
                           &allow-other-keys)
    "Launch program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on
Windows) _asynchronously_.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the
default) designating the null device, the file at that path is used as
output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*, and
under SLIME will be on your *inferior-lisp* buffer.  If it's T, output
goes to your current *STANDARD-OUTPUT* stream.  If it's :STREAM, a new
stream will be made available that can be accessed via
PROCESS-INFO-OUTPUT and read from. Otherwise, OUTPUT should be a value
that the underlying lisp implementation knows how to handle.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT. T designates the *ERROR-OUTPUT*,
:OUTPUT means redirecting the error output to the output stream,
and :STREAM causes a stream to be made available via
PROCESS-INFO-ERROR-OUTPUT.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that T designates the
*STANDARD-INPUT* and a stream requested through the :STREAM keyword
would be available through PROCESS-INFO-INPUT.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on to your Lisp
implementation, when applicable, for creation of the output stream.

LAUNCH-PROGRAM returns a PROCESS-INFO object."
    #-(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (progn command keys input output error-output directory element-type external-format
           if-input-does-not-exist if-output-exists if-error-output-exists ;; ignore
           (not-implemented-error 'launch-program))
    #+allegro
    (when (some #'(lambda (stream)
                    (and (streamp stream)
                         (not (file-stream-p stream))))
                (list input output error-output))
      (parameter-error "~S: Streams passed as I/O parameters need to be file streams on this lisp"
                       'launch-program))
    #+(or abcl clisp lispworks)
    (when (some #'streamp (list input output error-output))
      (parameter-error "~S: I/O parameters cannot be foreign streams on this lisp"
                       'launch-program))
    #+clisp
    (unless (eq error-output :interactive)
      (parameter-error "~S: The only admissible value for ~S is ~S on this lisp"
                       'launch-program :error-output :interactive))
    #+ecl
    (when (some #'(lambda (stream)
                    (and (streamp stream)
                         (not (file-or-synonym-stream-p stream))))
                (list input output error-output))
      (parameter-error "~S: Streams passed as I/O parameters need to be (synonymous with) file streams on this lisp"
                       'launch-program))
    ;; see comments for these functions
    (%handle-if-does-not-exist input if-input-does-not-exist)
    (%handle-if-exists output if-output-exists)
    (%handle-if-exists error-output if-error-output-exists)
    #+(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (let* ((%command (%normalize-command command))
           (%input (%normalize-io-specifier input :input))
           (%output (%normalize-io-specifier output :output))
           (%error-output (%normalize-io-specifier error-output :error-output))
           #+(and allegro os-windows)
           (interactive (%interactivep input output error-output))
           (process*
             (nest
              #-(or allegro mkcl sbcl) (with-current-directory (directory))
              #+(or allegro ecl lispworks mkcl) (multiple-value-list)
              (apply
               #+abcl #'sys:run-program
               #+allegro 'excl:run-shell-command
               #+(and allegro os-unix) (coerce (cons (first %command) %command) 'vector)
               #+(and allegro os-windows) %command
               #+clozure 'ccl:run-program
               #+(or cmucl ecl scl) 'ext:run-program
               #+lispworks 'system:run-shell-command
               #+lispworks (cons "/usr/bin/env" %command) ; LW wants a full path
               #+mkcl 'mk-ext:run-program
               #+sbcl 'sb-ext:run-program
               (append
                #+(or abcl clozure cmucl ecl mkcl sbcl scl) `(,(car %command) ,(cdr %command))
                `(:input ,%input :output ,%output
                  #.(or #+(or allegro lispworks) :error-output :error) ,%error-output
                  :wait nil :element-type ,element-type :external-format ,external-format
                  :if-input-does-not-exist :error
                  :if-output-exists :append
                  #-(or allegro lispworks) :if-error-exists
                  #+(or allegro lispworks) :if-error-output-exists :append
                  :allow-other-keys t)
                #+allegro `(:directory ,directory)
                #+(and allegro os-windows) `(:show-window ,(if interactive nil :hide))
                #+lispworks `(:save-exit-status t)
                #+mkcl `(:directory ,(native-namestring directory))
                #+sbcl `(:search t)
                #-sbcl keys ;; on SBCL, don't pass :directory nil but remove it from the keys
                #+sbcl (if directory keys (remove-plist-key :directory keys))))))
           (process-info (make-instance 'process-info)))
      (labels ((prop (key value) (setf (slot-value process-info key) value)))
        #+allegro
        (cond
          (separate-streams
           (destructuring-bind (in out err pid) process*
             (prop 'process pid)
             (when (eq input :stream) (prop 'input-stream in))
             (when (eq output :stream) (prop 'output-stream out))
             (when (eq error-output :stream) (prop 'error-stream err))))
          (t
           (prop 'process (third process*))
           (let ((x (first process*)))
             (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
               (0)
               (1 (prop 'input-stream x))
               (2 (prop 'output-stream x))
               (3 (prop 'bidir-stream x))))
           (when (eq error-output :stream)
             (prop 'error-stream (second process*)))))
        #+(or abcl clozure cmucl sbcl scl)
        (progn
          (prop 'process process*)
          (when (eq input :stream)
            (prop 'input-stream
                  #+abcl (symbol-call :sys :process-input process*)
                  #+clozure (ccl:external-process-input-stream process*)
                  #+(or cmucl scl) (ext:process-input process*)
                  #+sbcl (sb-ext:process-input process*)))
          (when (eq output :stream)
            (prop 'output-stream
                  #+abcl (symbol-call :sys :process-output process*)
                  #+clozure (ccl:external-process-output-stream process*)
                  #+(or cmucl scl) (ext:process-output process*)
                  #+sbcl (sb-ext:process-output process*)))
          (when (eq error-output :stream)
            (prop 'error-output-stream
                  #+abcl (symbol-call :sys :process-error process*)
                  #+clozure (ccl:external-process-error-stream process*)
                  #+(or cmucl scl) (ext:process-error process*)
                  #+sbcl (sb-ext:process-error process*))))
        #+(or ecl mkcl)
        (destructuring-bind #+ecl (stream code process) #+mkcl (stream process code) process*
          (declare (ignore code))
          (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
            (unless (zerop mode)
              (prop (case mode (1 'input-stream) (2 'output-stream) (3 'bidir-stream)) stream)))
          (prop 'process process))
        #+lispworks
        (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
          (if (or (plusp mode) (eq error-output :stream))
              (destructuring-bind (io err pid) process*
                #+lispworks7+ (declare (ignore pid))
                (prop 'process #+lispworks7+ io #-lispworks7+ pid)
                (when (plusp mode)
                  (prop (ecase mode
                          (1 'input-stream)
                          (2 'output-stream)
                          (3 'bidir-stream)) io))
                (when (eq error-output :stream)
                  (prop 'error-stream err)))
              ;; lispworks6 returns (pid), lispworks7 returns (io err pid) of which we keep io
              (prop 'process (first process*)))))
      process-info))

  (defun %run-program (command &rest keys &key &allow-other-keys)
    "DEPRECATED. Use LAUNCH-PROGRAM instead."
    (apply 'launch-program command keys))

  (defun process-info-error-output (process-info)
    (slot-value process-info 'error-output-stream))
  (defun process-info-input (process-info)
    (or (slot-value process-info 'bidir-stream)
        (slot-value process-info 'input-stream)))
  (defun process-info-output (process-info)
    (or (slot-value process-info 'bidir-stream)
        (slot-value process-info 'output-stream)))

  (defun process-info-pid (process-info)
    (let ((process (slot-value process-info 'process)))
      (declare (ignorable process))
      #+abcl (symbol-call :sys :process-pid process)
      #+allegro process
      #+clozure (ccl:external-process-id process)
      #+ecl (ext:external-process-pid process)
      #+(or cmucl scl) (ext:process-pid process)
      #+lispworks7+ (sys:pipe-pid process)
      #+(and lispworks (not lispworks7+)) process
      #+mkcl (mkcl:process-id process)
      #+sbcl (sb-ext:process-pid process)
      #-(or abcl allegro clozure cmucl ecl mkcl lispworks sbcl scl)
      (not-implemented-error 'process-info-pid)))

  (defun %process-status (process-info)
    (if-let (exit-code (slot-value process-info 'exit-code))
      (return-from %process-status
        (if-let (signal-code (slot-value process-info 'signal-code))
          (values :signaled signal-code)
          (values :exited exit-code))))
    #-(or allegro clozure cmucl ecl lispworks mkcl sbcl scl)
    (not-implemented-error '%process-status)
    (if-let (process (slot-value process-info 'process))
      (multiple-value-bind (status code)
          (progn
            #+allegro (multiple-value-bind (exit-code pid signal)
                          (sys:reap-os-subprocess :pid process :wait nil)
                        (assert pid)
                        (cond ((null exit-code) :running)
                              ((null signal) (values :exited exit-code))
                              (t (values :signaled signal))))
            #+clozure (ccl:external-process-status process)
            #+(or cmucl scl) (let ((status (ext:process-status process)))
                               (values status (if (member status '(:exited :signaled))
                                                  (ext:process-exit-code process))))
            #+ecl (ext:external-process-status process)
            #+lispworks
            ;; a signal is only returned on LispWorks 7+
            (multiple-value-bind (exit-code signal)
                (funcall #+lispworks7+ #'sys:pipe-exit-status
                         #-lispworks7+ #'sys:pid-exit-status
                         process :wait nil)
              (cond ((null exit-code) :running)
                    ((null signal) (values :exited exit-code))
                    (t (values :signaled signal))))
            #+mkcl (let ((status (mk-ext:process-status process))
                         (code (mk-ext:process-exit-code process)))
                     (if (stringp code)
                         (values :signaled (%mkcl-signal-to-number code))
                         (values status code)))
            #+sbcl (let ((status (sb-ext:process-status process)))
                     (values status (if (member status '(:exited :signaled))
                                        (sb-ext:process-exit-code process)))))
        (case status
          (:exited (setf (slot-value process-info 'exit-code) code))
          (:signaled (let ((%code (%signal-to-exit-code code)))
                       (setf (slot-value process-info 'exit-code) %code
                             (slot-value process-info 'signal-code) code))))
        (values status code))))

  (defun process-alive-p (process-info)
    "Check if a process has yet to exit."
    (unless (slot-value process-info 'exit-code)
      #+abcl (sys:process-alive-p (slot-value process-info 'process))
      #+(or cmucl scl) (ext:process-alive-p (slot-value process-info 'process))
      #+sbcl (sb-ext:process-alive-p (slot-value process-info 'process))
      #-(or abcl cmucl sbcl scl) (member (%process-status process-info)
                                         '(:running :sleeping))))

  (defun wait-process (process-info)
    "Wait for the process to terminate, if it is still running.
Otherwise, return immediately. An exit code (a number) will be
returned, with 0 indicating success, and anything else indicating
failure. If the process exits after receiving a signal, the exit code
will be the sum of 128 and the (positive) numeric signal code. A second
value may be returned in this case: the numeric signal code itself.
Any asynchronously spawned process requires this function to be run
before it is garbage-collected in order to free up resources that
might otherwise be irrevocably lost."
    (if-let (exit-code (slot-value process-info 'exit-code))
      (if-let (signal-code (slot-value process-info 'signal-code))
        (values exit-code signal-code)
        exit-code)
      (let ((process (slot-value process-info 'process)))
        #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
        (not-implemented-error 'wait-process)
        (when process
          ;; 1- wait
          #+clozure (ccl::external-process-wait process)
          #+(or cmucl scl) (ext:process-wait process)
          #+sbcl (sb-ext:process-wait process)
          ;; 2- extract result
          (multiple-value-bind (exit-code signal-code)
              (progn
                #+abcl (sys:process-wait process)
                #+allegro (multiple-value-bind (exit-code pid signal)
                              (sys:reap-os-subprocess :pid process :wait t)
                            (assert pid)
                            (values exit-code signal))
                #+clozure (multiple-value-bind (status code)
                              (ccl:external-process-status process)
                            (if (eq status :signaled)
                                (values nil code)
                                code))
                #+(or cmucl scl) (let ((status (ext:process-status process))
                                       (code (ext:process-exit-code process)))
                                   (if (eq status :signaled)
                                       (values nil code)
                                       code))
                #+ecl (multiple-value-bind (status code)
                          (ext:external-process-wait process t)
                        (if (eq status :signaled)
                            (values nil code)
                            code))
                #+lispworks (funcall #+lispworks7+ #'sys:pipe-exit-status
                                     #-lispworks7+ #'sys:pid-exit-status
                                     process :wait t)
                #+mkcl (let ((code (mkcl:join-process process)))
                         (if (stringp code)
                             (values nil (%mkcl-signal-to-number code))
                             code))
                #+sbcl (let ((status (sb-ext:process-status process))
                             (code (sb-ext:process-exit-code process)))
                         (if (eq status :signaled)
                             (values nil code)
                             code)))
            (if signal-code
                (let ((%exit-code (%signal-to-exit-code signal-code)))
                  (setf (slot-value process-info 'exit-code) %exit-code
                        (slot-value process-info 'signal-code) signal-code)
                  (values %exit-code signal-code))
                (progn (setf (slot-value process-info 'exit-code) exit-code)
                       exit-code)))))))

  (defun %check-result (exit-code &key command process ignore-error-status)
    (unless ignore-error-status
      (unless (eql exit-code 0)
        (cerror "IGNORE-ERROR-STATUS"
                'subprocess-error :command command :code exit-code :process process)))
    exit-code)

  (defun close-streams (process-info)
    "Close any stream that the process might own. Needs to be run
whenever streams were requested by passing :stream to :input, :output,
or :error-output."
    (dolist (stream
              (cons (slot-value process-info 'error-output-stream)
                    (if-let (bidir-stream (slot-value process-info 'bidir-stream))
                      (list bidir-stream)
                      (list (slot-value process-info 'input-stream)
                            (slot-value process-info 'output-stream)))))
      (when stream (close stream))))

  (defun %call-with-program-io (gf tval stream-easy-p fun direction spec activep returner
                                &key element-type external-format &allow-other-keys)
    ;; handle redirection for run-program and system
    ;; SPEC is the specification for the subprocess's input or output or error-output
    ;; TVAL is the value used if the spec is T
    ;; GF is the generic function to call to handle arbitrary values of SPEC
    ;; STREAM-EASY-P is T if we're going to use a RUN-PROGRAM that copies streams in the background
    ;; (it's only meaningful on CMUCL, SBCL, SCL that actually do it)
    ;; DIRECTION is :INPUT, :OUTPUT or :ERROR-OUTPUT for the direction of this io argument
    ;; FUN is a function of the new reduced spec and an activity function to call with a stream
    ;; when the subprocess is active and communicating through that stream.
    ;; ACTIVEP is a boolean true if we will get to run code while the process is running
    ;; ELEMENT-TYPE and EXTERNAL-FORMAT control what kind of temporary file we may open.
    ;; RETURNER is a function called with the value of the activity.
    ;; --- TODO (fare@tunes.org): handle if-output-exists and such when doing it the hard way.
    (declare (ignorable stream-easy-p))
    (let* ((actual-spec (if (eq spec t) tval spec))
           (activity-spec (if (eq actual-spec :output)
                              (ecase direction
                                ((:input :output)
                                 (error "~S not allowed as a ~S ~S spec"
                                        :output 'run-program direction))
                                ((:error-output)
                                 nil))
                              actual-spec)))
      (labels ((activity (stream)
                 (call-function returner (call-stream-processor gf activity-spec stream)))
               (easy-case ()
                 (funcall fun actual-spec nil))
               (hard-case ()
                 (if activep
                     (funcall fun :stream #'activity)
                     (with-temporary-file (:pathname tmp)
                       (ecase direction
                         (:input
                          (with-output-file (s tmp :if-exists :overwrite
                                               :external-format external-format
                                               :element-type element-type)
                            (activity s))
                          (funcall fun tmp nil))
                         ((:output :error-output)
                          (multiple-value-prog1 (funcall fun tmp nil)
                            (with-input-file (s tmp
                                               :external-format external-format
                                               :element-type element-type)
                              (activity s)))))))))
        (typecase activity-spec
          ((or null string pathname (eql :interactive))
           (easy-case))
          #+(or cmucl (and sbcl os-unix) scl) ;; streams are only easy on implementations that try very hard
          (stream
           (if stream-easy-p (easy-case) (hard-case)))
          (t
           (hard-case))))))

  (defmacro place-setter (place)
    (when place
      (let ((value (gensym)))
        `#'(lambda (,value) (setf ,place ,value)))))

  (defmacro with-program-input (((reduced-input-var
                                  &optional (input-activity-var (gensym) iavp))
                                 input-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'vomit-output-stream *standard-input* ,stream-easy-p
            #'(lambda (,reduced-input-var ,input-activity-var)
                ,@(unless iavp `((declare (ignore ,input-activity-var))))
                ,@body)
            :input ,input-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-output (((reduced-output-var
                                  &optional (output-activity-var (gensym) oavp))
                                  output-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *standard-output* ,stream-easy-p
            #'(lambda (,reduced-output-var ,output-activity-var)
                ,@(unless oavp `((declare (ignore ,output-activity-var))))
                ,@body)
            :output ,output-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-error-output (((reduced-error-output-var
                                         &optional (error-output-activity-var (gensym) eoavp))
                                        error-output-form &key setf stream-easy-p active keys)
                                       &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *error-output* ,stream-easy-p
            #'(lambda (,reduced-error-output-var ,error-output-activity-var)
                ,@(unless eoavp `((declare (ignore ,error-output-activity-var))))
                ,@body)
            :error-output ,error-output-form ,active (place-setter ,setf) ,keys))

  (defun %use-launch-program (command &rest keys
                           &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using LAUNCH-PROGRAM
    #+(or cormanlisp gcl (and lispworks os-windows) mcl xcl)
    (progn
      command keys input output error-output ignore-error-status ;; ignore
      (not-implemented-error '%use-launch-program))
    (when (member :stream (list input output error-output))
      (parameter-error "~S: ~S is not allowed as synchronous I/O redirection argument"
                       'run-program :stream))
    (let* ((active-input-p (%active-io-specifier-p input))
           (active-output-p (%active-io-specifier-p output))
           (active-error-output-p (%active-io-specifier-p error-output))
           (activity
             (cond
               (active-output-p :output)
               (active-input-p :input)
               (active-error-output-p :error-output)
               (t nil)))
           output-result error-output-result exit-code process-info)
      (with-program-output ((reduced-output output-activity)
                            output :keys keys :setf output-result
                            :stream-easy-p t :active (eq activity :output))
        (with-program-error-output ((reduced-error-output error-output-activity)
                                    error-output :keys keys :setf error-output-result
                                    :stream-easy-p t :active (eq activity :error-output))
          (with-program-input ((reduced-input input-activity)
                               input :keys keys
                               :stream-easy-p t :active (eq activity :input))
            (setf process-info
                  (apply 'launch-program command
                         :input reduced-input :output reduced-output
                         :error-output (if (eq error-output :output) :output reduced-error-output)
                         keys))
            (labels ((get-stream (stream-name &optional fallbackp)
                       (or (slot-value process-info stream-name)
                           (when fallbackp
                             (slot-value process-info 'bidir-stream))))
                     (run-activity (activity stream-name &optional fallbackp)
                       (if-let (stream (get-stream stream-name fallbackp))
                         (funcall activity stream)
                         (error 'subprocess-error
                                :code `(:missing ,stream-name)
                                :command command :process process-info))))
              (unwind-protect
                   (ecase activity
                     ((nil))
                     (:input (run-activity input-activity 'input-stream t))
                     (:output (run-activity output-activity 'output-stream t))
                     (:error-output (run-activity error-output-activity 'error-output-stream)))
                (close-streams process-info)
                (setf exit-code (wait-process process-info)))))))
      (%check-result exit-code
                     :command command :process process-info
                     :ignore-error-status ignore-error-status)
      (values output-result error-output-result exit-code)))

  (defun %normalize-system-command (command) ;; helper for %USE-SYSTEM
    (etypecase command
      (string
       (os-cond
        ((os-windows-p)
         #+(or allegro clisp)
         (strcat (%cmd-shell-pathname) " /c " command)
         #-(or allegro clisp) command)
        (t command)))
      (list (escape-shell-command
             (os-cond
              ((os-unix-p) (cons "exec" command))
              ((os-windows-p)
               #+(or allegro sbcl clisp)
               (cons (%cmd-shell-pathname) (cons "/c" command))
               #-(or allegro sbcl clisp) command)
              (t command))))))

  (defun %redirected-system-command (command in out err directory) ;; helper for %USE-SYSTEM
    (flet ((redirect (spec operator)
             (let ((pathname
                     (typecase spec
                       (null (null-device-pathname))
                       (string (parse-native-namestring spec))
                       (pathname spec)
                       ((eql :output)
                        (unless (equal operator " 2>>")
                          (parameter-error "~S: only the ~S argument can be ~S"
                                           'run-program :error-output :output))
                        (return-from redirect '(" 2>&1"))))))
               (when pathname
                 (list operator " "
                       (escape-shell-token (native-namestring pathname)))))))
      (let* ((redirections (append (redirect in " <") (redirect out " >>") (redirect err " 2>>")))
             (normalized (%normalize-system-command command))
             (directory (or directory #+(or abcl xcl) (getcwd)))
             (chdir (when directory
                      (let ((dir-arg (escape-shell-token (native-namestring directory))))
                        (os-cond
                         ((os-unix-p) `("cd " ,dir-arg " ; "))
                         ((os-windows-p) `("cd /d " ,dir-arg " & ")))))))
        (reduce/strcat
         (os-cond
          ((os-unix-p) `(,@(when redirections `("exec " ,@redirections " ; ")) ,@chdir ,normalized))
          ((os-windows-p) `(,@chdir ,@redirections " " ,normalized)))))))

  (defun %system (command &rest keys &key directory
                                       input if-input-does-not-exist
                                       output if-output-exists
                                       error-output if-error-output-exists
                                       &allow-other-keys)
    "A portable abstraction of a low-level call to libc's system()."
    (declare (ignorable directory keys))
    ;; see comments for these functions
    (%handle-if-does-not-exist input if-input-does-not-exist)
    (%handle-if-exists output if-output-exists)
    (%handle-if-exists error-output if-error-output-exists)
    #+(or allegro clozure cmucl (and lispworks os-unix) sbcl scl)
    (wait-process
     (apply 'launch-program (%normalize-system-command command) :wait t keys))
    #+(or abcl clasp clisp cormanlisp ecl gcl genera (and lispworks os-windows) mkcl xcl)
    (let ((%command (%redirected-system-command command input output error-output directory)))
      #+(and lispworks os-windows)
      (system:call-system %command :current-directory directory :wait t)
      #+clisp
      (let ((raw-exit-code
             (or
              #.`(#+os-windows ,@'(ext:run-shell-command %command)
                  #+os-unix ,@'(ext:run-program "/bin/sh" :arguments `("-c" ,%command))
                  :wait t :input :terminal :output :terminal)
              0)))
        (if (minusp raw-exit-code)
            (- 128 raw-exit-code)
            raw-exit-code))
      #-(or clisp (and lispworks os-windows))
      (with-current-directory ((os-cond ((not (os-unix-p)) directory)))
        #+abcl (ext:run-shell-command %command) ;; FIXME: deprecated
        #+cormanlisp (win32:system %command)
        #+(or clasp ecl) (let ((*standard-input* *stdin*)
                               (*standard-output* *stdout*)
                               (*error-output* *stderr*))
                           (ext:system %command))
        #+gcl (system:system %command)
        #+genera (not-implemented-error '%system)
        #+mcl (ccl::with-cstrs ((%%command %command)) (_system %%command))
        #+mkcl (mkcl:system %command)
        #+xcl (system:%run-shell-command %command))))

  (defun %use-system (command &rest keys
                      &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %system
    (let (output-result error-output-result exit-code)
      (with-program-output ((reduced-output)
                            output :keys keys :setf output-result)
        (with-program-error-output ((reduced-error-output)
                                    error-output :keys keys :setf error-output-result)
          (with-program-input ((reduced-input) input :keys keys)
            (setf exit-code (apply '%system command
                                   :input reduced-input :output reduced-output
                                   :error-output reduced-error-output keys)))))
      (%check-result exit-code
                     :command command
                     :ignore-error-status ignore-error-status)
      (values output-result error-output-result exit-code)))

  (defun run-program (command &rest keys
                       &key ignore-error-status (force-shell nil force-shell-suppliedp)
                         input (if-input-does-not-exist :error)
                         output (if-output-exists :supersede)
                         error-output (if-error-output-exists :supersede)
                         (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                         (external-format *utf-8-external-format*)
                      &allow-other-keys)
    "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows);
_synchronously_ process its output as specified and return the processing results
when the program and its output processing are complete.

Always call a shell (rather than directly execute the command when possible)
if FORCE-SHELL is specified.  Similarly, never call a shell if FORCE-SHELL is
specified to be NIL.

Signal a continuable SUBPROCESS-ERROR if the process wasn't successful (exit-code 0),
unless IGNORE-ERROR-STATUS is specified.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the default)
designating the null device, the file at that path is used as output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*,
and under SLIME will be on your *inferior-lisp* buffer.
If it's T, output goes to your current *STANDARD-OUTPUT* stream.
Otherwise, OUTPUT should be a value that is a suitable first argument to
SLURP-INPUT-STREAM (qv.), or a list of such a value and keyword arguments.
In this case, RUN-PROGRAM will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to SLURP-INPUT-STREAM,
using OUTPUT as the first argument (or the first element of OUTPUT, and the rest as keywords).
The primary value resulting from that call (or NIL if no call was needed)
will be the first value returned by RUN-PROGRAM.
E.g., using :OUTPUT :STRING will have it return the entire output stream as a string.
And using :OUTPUT '(:STRING :STRIPPED T) will have it return the same string
stripped of any ending newline.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT, except that the resulting value is returned
as the second value of RUN-PROGRAM. T designates the *ERROR-OUTPUT*.
Also :OUTPUT means redirecting the error output to the output stream,
in which case NIL is returned.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that VOMIT-OUTPUT-STREAM is used,
no value is returned, and T designates the *STANDARD-INPUT*.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel in parallel with the subprocess,
depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

RUN-PROGRAM returns 3 values:
0- the result of the OUTPUT slurping if any, or NIL
1- the result of the ERROR-OUTPUT slurping if any, or NIL
2- either 0 if the subprocess exited with success status,
or an indication of failure via the EXIT-CODE of the process"
    (declare (ignorable ignore-error-status))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'run-program)
    ;; Per doc string, set FORCE-SHELL to T if we get command as a string.
    ;; But don't override user's specified preference. [2015/06/29:rpg]
    (when (stringp command)
      (unless force-shell-suppliedp
        #-(and sbcl os-windows) ;; force-shell t isn't working properly on windows as of sbcl 1.2.16
        (setf force-shell t)))
    (apply (if (or force-shell
                   #+(or clasp clisp) t
                   ;; A race condition in ECL <= 16.0.0 prevents using ext:run-program
                   #+ecl #.(if-let (ver (parse-version (lisp-implementation-version)))
                                   (lexicographic<= '< ver '(16 0 1)))
                   #+(and lispworks os-unix) (%interactivep input output error-output)
                   #+(or cormanlisp gcl (and lispworks os-windows) mcl xcl) t)
               '%use-system '%use-launch-program)
           command
           :input input
           :output output
           :error-output error-output
           :if-input-does-not-exist if-input-does-not-exist
           :if-output-exists if-output-exists
           :if-error-output-exists if-error-output-exists
           :element-type element-type :external-format external-format
           keys))

  ;; WARNING: For signals other than SIGTERM and SIGKILL this may not
  ;; do what you expect it to. Sending SIGSTOP to a process spawned
  ;; via LAUNCH-PROGRAM, e.g., will stop the shell /bin/sh that is used
  ;; to run the command (via `sh -c command`) but not the actual
  ;; command.
  #+os-unix
  (defun %posix-send-signal (process-info signal)
    #+allegro (excl.osi:kill (slot-value process-info 'process) signal)
    #+clozure (ccl:signal-external-process (slot-value process-info 'process)
                                           signal :error-if-exited nil)
    #+(or cmucl scl) (ext:process-kill (slot-value process-info 'process) signal)
    #+sbcl (sb-ext:process-kill (slot-value process-info 'process) signal)
    #-(or allegro clozure cmucl sbcl scl)
    (if-let (pid (process-info-pid process-info))
      (run-program (format nil "kill -~a ~a" signal pid)
                   :ignore-error-status t)))

  ;;; this function never gets called on Windows, but the compiler cannot tell
  ;;; that. [2016/09/25:rpg]
  #+os-windows
  (defun %posix-send-signal (process-info signal)
    (declare (ignore process-info signal))
    (values))

  (defun terminate-process (process-info &key urgent)
    "Cause the process to exit. To that end, the process may or may
not be sent a signal, which it will find harder (or even impossible)
to ignore if URGENT is T. On some platforms, it may also be subject to
race conditions."
    (declare (ignorable urgent))
    #+abcl (sys:process-kill (slot-value process-info 'process))
    ;; On ECL, this will only work on versions later than 2016-09-06,
    ;; but we still want to compile on earlier versions, so we use symbol-call
    #+ecl (symbol-call :ext :terminate-process (slot-value process-info 'process) urgent)
    #+lispworks7+ (sys:pipe-kill-process (slot-value process-info 'process))
    #+mkcl (mk-ext:terminate-process (slot-value process-info 'process)
                                     :force urgent)
    #-(or abcl ecl lispworks7+ mkcl)
    (os-cond
     ((os-unix-p) (%posix-send-signal process-info (if urgent 9 15)))
     ((os-windows-p) (if-let (pid (process-info-pid process-info))
                       (run-program (format nil "taskkill ~:[~;/f ~]/pid ~a"
                                            urgent pid)
                                    :ignore-error-status t)))
     (t (not-implemented-error 'terminate-process)))))

