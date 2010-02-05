(in-package #:common-lisp-user)

#+allegro
(setf excl:*warn-on-nested-reader-conditionals* nil)

;;; code adapted from cl-launch (any errors in transcription are mine!)
;; http://www.cliki.net/cl-launch
(defun leave-lisp (message return)
  (when message
    (format *error-output* message))
  #+allegro
  (excl:exit return)
  #+clisp
  (ext:quit return)
  #+(or cmu scl)
  (unix:unix-exit code)
  #+ecl
  (si:quit return)
  #+gcl
  (lisp:quit code)
  #+lispworks
  (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+(or openmcl mcl)
  (ccl::quit return)
  #+sbcl
  (sb-ext:quit :unix-status return)
  (error "Don't know how to quit Lisp; wanting to use exit code ~a" return))


(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (handler-case
      (progn (funcall thunk)
             (leave-lisp "~&Script succeeded~%" 0))
    (error (c)
      (format *error-output* "~a" c)
      (if (ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
          (break)
          (leave-lisp "~&Script failed~%" 1)))))
