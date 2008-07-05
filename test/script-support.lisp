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

(defmacro exit-on-error (&body body)
  `(handler-case 
      (progn ,@body
	     (leave-lisp "Script succeeded" 0))
    (error (c)
      (format *error-output* "~a" c)
      (leave-lisp "Script failed" 1))))
