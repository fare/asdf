;;;; -------------------------------------------------------------------------
;;;; Starting, Stopping, Dumping a Lisp image

(asdf/package:define-package :asdf/image
  (:recycle :asdf/image :xcvb-driver)
  (:use :common-lisp :asdf/utility :asdf/pathname :asdf/stream :asdf/os)
  (:export
   #:*dumped* #:raw-command-line-arguments #:*command-line-arguments*
   #:command-line-arguments #:raw-command-line-arguments #:setup-command-line-arguments
   #:*debugging* #:*post-image-restart* #:*entry-point*
   #:quit #:die #:raw-print-backtrace #:print-backtrace #:print-condition-backtrace
   #:bork #:with-coded-exit #:shell-boolean
   #:register-image-resume-hook #:register-image-dump-hook
   #:call-image-resume-hook #:call-image-dump-hook
   #:initialize-asdf-utilities
   #:resume-image #:run-resumed-program #:dump-image 
))
(in-package :asdf/image)

(defvar *debugging* nil
  "Shall we print extra debugging information?")

(defvar *command-line-arguments* nil
  "Command-line arguments")

(defvar *dumped* nil
  "Is this a dumped image? As a standalone executable?")

(defvar *image-resume-hook* nil
  "Functions to call (in reverse order) when the image is resumed")

(defvar *image-dump-hook* nil
  "Functions to call (in order) when before an image is dumped")

(defvar *post-image-restart* nil
  "a string containing forms to read and evaluate when the image is restarted,
but before the entry point is called.")

(defvar *entry-point* nil
  "a function with which to restart the dumped image when execution is resumed from it.")



;;; Exiting properly or im-
(defun* quit (&optional (code 0) (finish-output t))
  "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
  (when finish-output ;; essential, for ClozureCL, and for standard compliance.
    (finish-outputs))
  #+(or abcl xcl) (ext:quit :status code)
  #+allegro (excl:exit code :quiet t)
  #+clisp (ext:quit code)
  #+clozure (ccl:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+(or cmu scl) (unix:unix-exit code)
  #+ecl (si:quit code)
  #+gcl (lisp:quit code)
  #+genera (error "You probably don't want to Halt the Machine. (code: ~S)" code)
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+mcl (ccl:quit) ;; or should we use FFI to call libc's exit(3) ?
  #+mkcl (mk-ext:quit :exit-code code)
  #+sbcl #.(let ((exit (find-symbol* :exit :sb-ext nil))
		 (quit (find-symbol* :quit :sb-ext nil)))
	     (cond
	       (exit `(,exit :code code :abort (not finish-output)))
	       (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
  #-(or abcl allegro clisp clozure cmu ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S called with exit code ~S but there's no quitting on this implementation" 'quit code))

(defun* die (code format &rest arguments)
  "Die in error with some error message"
  (with-safe-io-syntax ()
    (ignore-errors
     (fresh-line *stderr*)
     (apply #'format *stderr* format arguments)
     (format! *stderr* "~&")))
  (quit code))

(defun* raw-print-backtrace (&key (stream *debug-io*) count)
  "Print a backtrace, directly accessing the implementation"
  (declare (ignorable stream count))
  #+allegro
  (let ((*terminal-io* stream)
        (*standard-output* stream)
        (tpl:*zoom-print-circle* *print-circle*)
        (tpl:*zoom-print-level* *print-level*)
        (tpl:*zoom-print-length* *print-length*))
    (tpl:do-command "zoom"
      :from-read-eval-print-loop nil
      :count t
      :all t))
  #+clisp
  (system::print-backtrace :out stream :limit count)
  #+(or clozure mcl)
  (let ((*debug-io* stream))
    (ccl:print-call-history :count count :start-frame-number 1)
    (finish-output stream))
  #+(or cmucl scl)
  (let ((debug:*debug-print-level* *print-level*)
        (debug:*debug-print-length* *print-length*))
    (debug:backtrace most-positive-fixnum stream))
  #+ecl
  (si::tpl-backtrace)
  #+lispworks
  (let ((dbg::*debugger-stack*
          (dbg::grab-stack nil :how-many (or count most-positive-fixnum)))
        (*debug-io* stream)
        (dbg:*debug-print-level* *print-level*)
        (dbg:*debug-print-length* *print-length*))
    (dbg:bug-backtrace nil))
  #+sbcl
  (sb-debug:backtrace
   #.(if (find-symbol* "*VERBOSITY*" "SB-DEBUG" nil) :stream '(or count most-positive-fixnum))
   stream))

(defun* print-backtrace (&rest keys &key stream count)
  (declare (ignore stream count))
  (with-safe-io-syntax (:package :cl)
    (let ((*print-readably* nil)
          (*print-circle* t)
          (*print-miser-width* 75)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pretty* t))
      (ignore-errors (apply 'raw-print-backtrace keys)))))

(defun* print-condition-backtrace (condition &key (stream *stderr*) count)
  ;; We print the condition *after* the backtrace,
  ;; for the sake of who sees the backtrace at a terminal.
  ;; It is up to the caller to print the condition *before*, with some context.
  (print-backtrace :stream stream :count count)
  (when condition
    (safe-format! stream "~&Above backtrace due to this condition:~%~A~&"
                  condition)))

(defun* bork (condition)
  "Depending on whether *DEBUGGING* is set, enter debugger or die"
  (safe-format! *stderr* "~&BORK:~%~A~%" condition)
  (cond
    (*debugging*
     (invoke-debugger condition))
    (t
     (print-condition-backtrace condition :stream *stderr*)
     (die 99 "~A" condition))))

(defun* call-with-coded-exit (thunk)
  (handler-bind ((error 'bork))
    (funcall thunk)
    (quit 0)))

(defmacro with-coded-exit ((&optional) &body body)
  "Run BODY, BORKing on error and otherwise exiting with a success status"
  `(call-with-coded-exit #'(lambda () ,@body)))

(defun* shell-boolean (x)
  "Quit with a return code that is 0 iff argument X is true"
  (quit (if x 0 1)))


;;; Using hooks

(defun* register-image-resume-hook (hook &optional (now t))
  (register-hook-function '*image-resume-hook* hook now))

(defun* register-image-dump-hook (hook &optional (now nil))
  (register-hook-function '*image-dump-hook* hook now))

(defun* call-image-resume-hook ()
  (call-functions (reverse *image-resume-hook*)))

(defun* call-image-dump-hook ()
  (call-functions *image-dump-hook*))


;;; Proper command-line arguments

(defun* raw-command-line-arguments ()
  "Find what the actual command line for this process was."
  #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
  #+allegro (sys:command-line-arguments) ; default: :application t
  #+clisp (coerce (ext:argv) 'list)
  #+clozure (ccl::command-line-arguments)
  #+(or cmu scl) extensions:*command-line-strings*
  #+ecl (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
  #+gcl si:*command-args*
  #+lispworks sys:*line-arguments-list*
  #+sbcl sb-ext:*posix-argv*
  #+xcl system:*argv*
  #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl xcl)
  (error "raw-command-line-arguments not implemented yet"))

(defun* command-line-arguments (&optional (arguments (raw-command-line-arguments)))
  "Extract user arguments from command-line invocation of current process.
Assume the calling conventions of an XCVB-generated script
if we are not called from a directly executable image dumped by XCVB."
  #+abcl arguments
  #-abcl
  (let* (#-(or sbcl allegro)
	 (arguments
	  (if (eq *dumped* :executable)
	      arguments
	      (member "--" arguments :test 'string-equal))))
    (rest arguments)))

(defun setup-command-line-arguments ()
  (setf *command-line-arguments* (command-line-arguments)))

(defun* resume-image (&key (post-image-restart *post-image-restart*)
                           (entry-point *entry-point*)
                           (image-resume-hook *image-resume-hook*))
  (call-functions image-resume-hook)
  (when post-image-restart
    (with-safe-io-syntax ()
      (let ((*read-eval* t))
        (eval-input post-image-restart))))
  (when entry-point
    (apply entry-point *command-line-arguments*)))

(defun* run-resumed-program ()
  (with-coded-exit ()
    (let ((ret (resume-program)))
      (if (typep ret 'integer)
          (quit ret)
          (quit 99)))))


;;; Dumping an image

#-(or ecl mkcl)
(defun* dump-image (filename &key output-name executable pre-image-dump post-image-restart entry-point package)
  (declare (ignorable filename output-name executable pre-image-dump post-image-restart entry-point))
  (setf *dumped* (if executable :executable t))
  (setf *package* (find-package (or package :cl-user)))
  (with-safe-io-syntax ()
    (let ((*read-eval* t))
      (when pre-image-dump (eval-input pre-image-dump))
      (setf *entry-point* (when entry-point (ensure-function entry-point)))
      (when post-image-restart (setf *post-image-restart* post-image-restart))))
  #-(or clisp clozure cmu lispworks sbcl)
  (when executable
    (error "Dumping an executable is not supported on this implementation! Aborting."))
  #+allegro
  (progn
    (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
    (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
  #+clisp
  (apply #'ext:saveinitmem filename
   :quiet t
   :start-package *package*
   :keep-global-handlers nil
   :executable (if executable 0 t) ;--- requires clisp 2.48 or later, still catches --clisp-x
   (when executable
     (list
      :norc t
      :script nil
      :init-function #'resume
      ;; :parse-options nil ;--- requires a non-standard patch to clisp.
      )))
  #+clozure
  (ccl:save-application filename :prepend-kernel t
                        :toplevel-function (when executable #'resume))
  #+(or cmu scl)
  (progn
   (ext:gc :full t)
   (setf ext:*batch-mode* nil)
   (setf ext::*gc-run-time* 0)
   (apply 'ext:save-lisp filename #+cmu :executable #+cmu t
          (when executable '(:init-function resume :process-command-line nil))))
  #+gcl
  (progn
   (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
   (si::save-system filename))
  #+lispworks
  (if executable
      (lispworks:deliver 'resume filename 0 :interface nil)
      (hcl:save-image filename :environment nil))
  #+sbcl
  (progn
    ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
   (setf sb-ext::*gc-run-time* 0)
   (apply 'sb-ext:save-lisp-and-die filename
    :executable t ;--- always include the runtime that goes with the core
    (when executable (list :toplevel #'resume :save-runtime-options t)))) ;--- only save runtime-options for standalone executables
  #-(or allegro clisp clozure cmu gcl lispworks sbcl scl)
  (die 98 "Can't dump ~S: asdf doesn't support image dumping with this Lisp implementation.~%" filename))


;;; Initial environmental hooks
(pushnew 'setup-temporary-directory *image-resume-hook*)
(pushnew 'setup-stderr *image-resume-hook*)
(pushnew 'setup-command-line-arguments *image-resume-hook*)
