;;;; -------------------------------------------------------------------------
;;;; Starting, Stopping, Dumping a Lisp image

(asdf/package:define-package :asdf/image
  (:recycle :asdf/image :xcvb-driver)
  (:use :common-lisp :asdf/package :asdf/utility :asdf/pathname :asdf/stream :asdf/os)
  (:export
   #:*image-dumped-p* #:raw-command-line-arguments #:*command-line-arguments*
   #:command-line-arguments #:raw-command-line-arguments #:setup-command-line-arguments
   #:*lisp-interaction*
   #:fatal-conditions #:fatal-condition-p #:handle-fatal-condition
   #:call-with-fatal-condition-handler #:with-fatal-condition-handler
   #:*image-restore-hook* #:*image-prelude* #:*image-entry-point*
   #:*image-postlude* #:*image-dump-hook*
   #:quit #:die #:raw-print-backtrace #:print-backtrace #:print-condition-backtrace
   #:shell-boolean-exit
   #:register-image-restore-hook #:register-image-dump-hook
   #:call-image-restore-hook #:call-image-dump-hook
   #:initialize-asdf-utilities #:restore-image #:dump-image #:create-image
))
(in-package :asdf/image)

(defvar *lisp-interaction* t
  "Is this an interactive Lisp environment, or is it batch processing?")

(defvar *command-line-arguments* nil
  "Command-line arguments")

(defvar *image-dumped-p* nil ; may matter as to how to get to command-line-arguments
  "Is this a dumped image? As a standalone executable?")

(defvar *image-restore-hook* nil
  "Functions to call (in reverse order) when the image is restored")

(defvar *image-prelude* nil
  "a form to evaluate, or string containing forms to read and evaluate
when the image is restarted, but before the entry point is called.")

(defvar *image-entry-point* nil
  "a function with which to restart the dumped image when execution is restored from it.")

(defvar *image-postlude* nil
  "a form to evaluate, or string containing forms to read and evaluate
before the image dump hooks are called and before the image is dumped.")

(defvar *image-dump-hook* nil
  "Functions to call (in order) when before an image is dumped")

(defvar *fatal-conditions* '(error)
  "conditions that cause the Lisp image to enter the debugger if interactive,
or to die if not interactive")


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

(defun fatal-condition-p (condition)
  (match-any-condition-p condition *fatal-conditions*))

(defun* handle-fatal-condition (condition)
  "Depending on whether *LISP-INTERACTION* is set, enter debugger or die"
  (cond
    (*lisp-interaction*
     (invoke-debugger condition))
    (t
     (safe-format! *stderr* "~&Fatal condition:~%~A~%" condition)
     (print-condition-backtrace condition :stream *stderr*)
     (die 99 "~A" condition))))

(defun* call-with-fatal-condition-handler (thunk)
  (handler-bind (((satisfies fatal-condition-p) #'handle-fatal-condition))
    (funcall thunk)))

(defmacro with-fatal-condition-handler ((&optional) &body body)
  `(call-with-fatal-condition-handler #'(lambda () ,@body)))

(defun* shell-boolean-exit (x)
  "Quit with a return code that is 0 iff argument X is true"
  (quit (if x 0 1)))


;;; Using image hooks

(defun* register-image-restore-hook (hook &optional (call-now-p t))
  (register-hook-function '*image-restore-hook* hook call-now-p))

(defun* register-image-dump-hook (hook &optional (call-now-p nil))
  (register-hook-function '*image-dump-hook* hook call-now-p))

(defun* call-image-restore-hook ()
  (call-functions (reverse *image-restore-hook*)))

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
	  (if (eq *image-dumped-p* :executable)
	      arguments
	      (member "--" arguments :test 'string-equal))))
    (rest arguments)))

(defun setup-command-line-arguments ()
  (setf *command-line-arguments* (command-line-arguments)))

(defun* restore-image (&key
                       ((:lisp-interaction *lisp-interaction*) *lisp-interaction*)
                       ((:restore-hook *image-restore-hook*) *image-restore-hook*)
                       ((:prelude *image-prelude*) *image-prelude*)
                       ((:entry-point *image-entry-point*) *image-entry-point*))
  (with-fatal-condition-handler ()
    (call-image-restore-hook)
    (standard-eval-thunk *image-prelude*)
    (let ((results (multiple-value-list
                    (if *image-entry-point*
                        (apply (ensure-function *image-entry-point*) *command-line-arguments*)
                        t))))
      (if *lisp-interaction*
          (apply 'values results)
          (shell-boolean-exit (first results))))))


;;; Dumping an image

#-(or ecl mkcl)
(defun* dump-image (filename &key output-name executable
                             ((:postlude *image-postlude*) *image-postlude*)
                             ((:dump-hook *image-dump-hook*) *image-dump-hook*))
  (declare (ignorable filename output-name executable))
  (setf *image-dumped-p* (if executable :executable t))
  (standard-eval-thunk *image-postlude*)
  (call-image-dump-hook)
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
      ;; :parse-options nil ;--- requires a non-standard patch to clisp.
      :norc t :script nil :init-function #'restore-image)))
  #+clozure
  (ccl:save-application filename :prepend-kernel t
                        :toplevel-function (when executable #'restore-image))
  #+(or cmu scl)
  (progn
   (ext:gc :full t)
   (setf ext:*batch-mode* nil)
   (setf ext::*gc-run-time* 0)
   (apply 'ext:save-lisp filename #+cmu :executable #+cmu t
          (when executable '(:init-function restore-image :process-command-line nil))))
  #+gcl
  (progn
   (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
   (si::save-system filename))
  #+lispworks
  (if executable
      (lispworks:deliver 'restore-image filename 0 :interface nil)
      (hcl:save-image filename :environment nil))
  #+sbcl
  (progn
    ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
   (setf sb-ext::*gc-run-time* 0)
   (apply 'sb-ext:save-lisp-and-die filename
    :executable t ;--- always include the runtime that goes with the core
    (when executable (list :toplevel #'restore-image :save-runtime-options t)))) ;--- only save runtime-options for standalone executables
  #-(or allegro clisp clozure cmu gcl lispworks sbcl scl)
  (die 98 "Can't dump ~S: asdf doesn't support image dumping with ~A.~%"
       filename (nth-value 1 (implementation-type))))


#+ecl
(defun create-image (destination object-files
                     &key kind output-name
                       (prelude () preludep) (entry-point () entry-point-p))
  ;; Is it meaningful to run these in the current environment?
  ;; only if we also track the object files that constitute the "current" image,
  ;; and otherwise simulate dump-image, including quitting at the end.
  ;; (standard-eval-thunk *image-postlude*) (call-image-dump-hook)
  (check-type kind (member :program :shared-library))
  (c::builder
   kind (pathname destination)
       :lisp-files object-files
       :init-name (c::compute-init-name (or output-name destination) :kind kind)
       :epilogue-code
       (when (eq kind :program)
         `(restore-image ;; default behavior would be (si::top-level)
           ,@(when preludep `(:prelude ',prelude))
           ,@(when entry-point-p `(:entry-point ',entry-point))))))


;;; Some universal image restore hooks
(map () 'register-image-restore-hook
     '(setup-temporary-directory setup-stderr setup-command-line-arguments))
