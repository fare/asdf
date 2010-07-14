(in-package #:common-lisp-user)

;; We can't use asdf:merge-pathnames* because ASDF isn't loaded yet.
;; We still want to work despite and host/device funkiness.
(defparameter *test-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *compile-file-truename*)))
(defparameter *asdf-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative :back) :defaults *test-directory*)
   *test-directory*))
(defparameter *asdf-lisp*
  (make-pathname :name "asdf" :type "lisp" :defaults *asdf-directory*))
(defparameter *asdf-fasl*
  (compile-file-pathname
   (let ((impl (string-downcase
                (or #+allegro
                    (ecase excl:*current-case-mode*
                      (:case-sensitive-lower :mlisp)
                      (:case-insensitive-upper :alisp))
                    #+armedbear :abcl
                    #+clisp :clisp
                    #+clozure :ccl
                    #+cmu :cmucl
                    #+corman :cormanlisp
                    #+digitool :mcl
                    #+ecl :ecl
                    #+gcl :gcl
                    #+lispworks :lispworks
                    #+sbcl :sbcl
                    #+scl scl))))
     (merge-pathnames
      (make-pathname :directory `(:relative "tmp" "fasls" ,impl)
                     :defaults *asdf-directory*)
      *asdf-lisp*))))

(defun load-asdf ()
  (load *asdf-fasl*))

#+allegro
(setf excl:*warn-on-nested-reader-conditionals* nil)

;;; code adapted from cl-launch (any errors in transcription are mine!)
;; http://www.cliki.net/cl-launch
(defun leave-lisp (message return)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-output *error-output*)
  (finish-output *standard-output*)
  #+allegro
  (excl:exit return)
  #+clisp
  (ext:quit return)
  #+(or cmu scl)
  (unix:unix-exit return)
  #+ecl
  (si:quit return)
  #+gcl
  (lisp:quit return)
  #+lispworks
  (lispworks:quit :status return :confirm nil :return nil :ignore-errors-p t)
  #+(or openmcl mcl)
  (ccl::quit return)
  #+sbcl
  (sb-ext:quit :unix-status return)
  #+abcl
  (ext:quit :status return)
  (error "Don't know how to quit Lisp; wanting to use exit code ~a" return))


(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (handler-bind
      ((error (lambda (c)
                (format *error-output* "~a" c)
                (cond
                  ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
                   (break))
                  (t
                   (format *error-output* "ABORTING:~% ~S~%" c)
                   #+sbcl (sb-debug:backtrace 69)
                   #+clozure (ccl:print-call-history :count 69 :start-frame-number 1)
                   #+clisp (system::print-backtrace)
                   (format *error-output* "ABORTING:~% ~S~%" c)
                   (leave-lisp "~&Script failed~%" 1))))))
    (funcall thunk)
    (leave-lisp "~&Script succeeded~%" 0)))
