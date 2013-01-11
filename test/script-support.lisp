(defpackage :asdf-test
  (:use :common-lisp)
  (:export
   #:*test-directory* #:*asdf-directory*
   #:load-asdf
   #:register-directory #:asdf-load
   #:load-asdf-lisp #:compile-asdf #:load-asdf-fasl #:compile-load-asdf #:load-asdf-system
   #:quit-on-error #:test-asdf
   #:assert-equal
   #:exit-lisp #:leave-lisp
   #:quietly))

(in-package :asdf-test)

(declaim (optimize (speed 2) (safety 3) #-(or allegro gcl) (debug 3)))
(proclaim '(optimize (speed 2) (safety 3) #-(or allegro gcl) (debug 3)))

;; NB: can't print anything because of forward-ref test.
;; (DBG "Evaluating asdf/test/script-support") 

;; We can't use asdf::merge-pathnames* because ASDF isn't loaded yet.
;; We still want to work despite and host/device funkiness.
(defparameter *test-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or #+gcl (truename system:*load-pathname*)
                               *load-truename* *compile-file-truename*)))
(defparameter *asdf-directory*
  (truename
   (merge-pathnames
    (make-pathname :directory '(#-gcl :relative #-gcl :back #+gcl :parent) :defaults *test-directory*)
    *test-directory*)))
(defparameter *asdf-lisp*
  (merge-pathnames
   (make-pathname :directory '(#-gcl :relative "build") :name "asdf" :type "lisp" :defaults *asdf-directory*)
   *asdf-directory*))
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
		    #+mkcl :mkcl
                    #+sbcl :sbcl
                    #+scl :scl
                    #+xcl :xcl))))
     (merge-pathnames
      (make-pathname :directory `(#-gcl :relative "fasls" ,impl)
                     :defaults *asdf-directory*)
      *asdf-lisp*))))

(defun load-old-asdf (tag)
  (let ((old-asdf
          (merge-pathnames
           (make-pathname :directory `(#-gcl :relative "build")
                          :name (format nil "asdf-~A" tag) :type "lisp"
                          :defaults *asdf-directory*)
           *asdf-directory*)))
    (handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
      (load old-asdf))))

(defvar *debug-symbols*
  '( #|:COMPILE-FILE* :perform-lisp-compilation|# ))

(defun configure-asdf ()
  (eval `(trace ,@(loop :for s :in *debug-symbols* :collect (find-symbol (string s) :asdf))))
  (funcall (find-symbol (string :initialize-source-registry) :asdf)
           `(:source-registry :ignore-inherited-configuration))
  (funcall (find-symbol (string :initialize-output-translations) :asdf)
           `(:output-translations
             (,*test-directory* (,*asdf-directory* "build/fasls" :implementation "test"))
             (t (,*asdf-directory* "build/fasls" :implementation "root"))
             :ignore-inherited-configuration))
  (let ((registry (find-symbol (string :*central-registry*) :asdf)))
    (set registry `(,*asdf-directory* ,*test-directory*))))

(defun touch-file (file &key (offset 0) timestamp)
  (let ((timestamp (or timestamp (+ offset (get-universal-time)))))
    (multiple-value-bind (sec min hr day month year) (decode-universal-time timestamp)
      (funcall (find-symbol (string :run-shell-command) :asdf)
               "touch -t ~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D.~2,'0D ~S"
               year month day hr min sec (namestring file)))))

(defun load-asdf ()
  (load *asdf-fasl*)
  (use-package :asdf :asdf-test)
  (import 'DBG :asdf)
  (configure-asdf)
  (setf *package* (find-package :asdf-test)))

(defun hash-table->alist (table)
  (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
    :collect (cons key value)))

(defun common-lisp-user::load-asdf ()
  (load-asdf))

#+allegro
(setf excl:*warn-on-nested-reader-conditionals* nil)

;;; code adapted from cl-launch http://www.cliki.net/cl-launch
(defun exit-lisp (return)
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
  #+mkcl
  (mk-ext:quit :exit-code return)
  #+sbcl #.(let ((exit (find-symbol "EXIT" :sb-ext))
                 (quit (find-symbol "QUIT" :sb-ext)))
             (cond
               (exit `(,exit :code return :abort t))
               (quit `(,quit :unix-status return :recklessly-p t))))
  #+(or abcl xcl)
  (ext:quit :status return)
  (error "Don't know how to quit Lisp; wanting to use exit code ~a" return))

(defun finish-outputs ()
  (loop :for s :in (list *standard-output* *error-output* *trace-output* *debug-io*)
        :do (finish-output s)))

(defun redirect-outputs ()
  (finish-outputs)
  (setf *error-output* *standard-output*
        *trace-output* *standard-output*))

(defun leave-lisp (message return)
  (finish-outputs)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-outputs)
  (exit-lisp return))

(defmacro assert-equal (x y)
  `(assert (equal ,x ,y) () "These two expressions are not equal:~% ~S evaluates to ~S~% ~S evaluates to ~S~%"
           ',x ,x ',y ,y))

(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_ASDF_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (redirect-outputs)
  (handler-bind
      ((error (lambda (c)
                (format *error-output* "~&~a~&" c)
                (cond
                  ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_ASDF_TEST"))
                   (break))
                  (t
                   (finish-output *standard-output*)
                   (finish-output *trace-output*)
                   (format *error-output* "~&ABORTING:~% ~A~%" c)
                   (finish-output *error-output*)
                   #+sbcl (sb-debug:backtrace 69)
                   #+clozure (ccl:print-call-history :count 69 :start-frame-number 1)
                   #+clisp (system::print-backtrace)
                   #+ecl (si::tpl-backtrace)
                   (format *error-output* "~&ABORTING:~% ~A~%" c)
                   (finish-output *error-output*)
                   (finish-output *standard-output*)
                   (finish-output *trace-output*)
                   (leave-lisp "Script failed" 1))))))
    (funcall thunk)
    (leave-lisp "Script succeeded" 0)))


;;; These are used by the upgrade tests

(defmacro quietly (&body body)
  `(call-quietly #'(lambda () ,@body)))

(defun call-quietly (thunk)
  (handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
    (funcall thunk)))

(defun load-asdf-lisp ()
  (load *asdf-lisp*))

#+(or gcl genera)
(unless (fboundp 'ensure-directories-exist)
  (defun ensure-directories-exist (path)
    #+genera (fs:create-directories-recursively (pathname path))
    #+gcl (lisp:system (format nil "mkdir -p ~S" (namestring (make-pathname :name nil :type nil :defaults path))))))

(defun compile-asdf (&optional (output *asdf-fasl*))
  (ensure-directories-exist *asdf-fasl*)
  ;; style warnings shouldn't abort the compilation [2010/02/03:rpg]
  (handler-bind (#+sbcl ((or sb-c::simple-compiler-note sb-kernel:redefinition-warning) #'muffle-warning)
                 #+(and ecl (not ecl-bytecmp))
                 ((or c:compiler-note c::compiler-debug-note
                      c:compiler-warning) ;; ECL emits more serious warnings than it should.
                   #'muffle-warning)
                 #+mkcl
                 ((or compiler:compiler-note)
                   #'muffle-warning)
                 #-(or cmu scl)
                 (style-warning
                   #'(lambda (w)
                       ;; escalate style-warnings to warnings - we don't want them.
                       (warn "Can you please fix ASDF to not emit style-warnings? Got a ~S:~%~A"
                             (type-of w) w)
                       (muffle-warning w))))
    (compile-file *asdf-lisp* :output-file output #-gcl :verbose #-gcl t :print t)))

(defun load-asdf-fasl ()
  (load *asdf-fasl*))

(defun compile-load-asdf ()
  ;; emulate the way asdf upgrades itself: load source, compile, load fasl.
  (load-asdf-lisp)
  (compile-asdf)
  (load-asdf-fasl))

(defun register-directory (dir)
  (pushnew dir (symbol-value (find-symbol (string :*central-registry*) :asdf))))

(defun asdf-load (x &key verbose)
  (let ((xoos (find-symbol (string :oos) :asdf))
        (xload-op (find-symbol (string :load-op) :asdf))
        (*load-print* verbose)
        (*load-verbose* verbose))
    (funcall xoos xload-op x :verbose verbose)))

(defun load-asdf-system (&rest keys)
  (quietly
   (register-directory *asdf-directory*)
   (apply 'asdf-load :asdf keys)))

(defun testing-asdf (thunk)
  (quit-on-error
   (quietly
    (funcall thunk)
    (register-directory *test-directory*)
    (asdf-load :test-module-depend))))

(defmacro test-asdf (&body body)
  `(testing-asdf #'(lambda () ,@body)))

(defmacro DBG (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of variable and their values, returns the last value"
  ;"if not in debugging mode, just compute and return last value"
  ; #-do-test (declare (ignore tag)) #-do-test (car (last exprs)) #+do-test
  (let ((res (gensym))(f (gensym)))
  `(let (,res (*print-readably* nil))
    (flet ((,f (fmt &rest args) (apply #'format *error-output* fmt args)))
      (,f "~&~A~%" ,tag)
      ,@(mapcan
         #'(lambda (x)
            `((,f "~&  ~S => " ',x)
              (,f "~{~S~^ ~}~%" (setf ,res (multiple-value-list ,x)))))
         exprs)
      (apply 'values ,res)))))

(pushnew :DBG *features*)

#+gcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (DBG :script-support *package* *test-directory* *asdf-directory* *asdf-lisp* *asdf-fasl*
       ))

#|
(DBG :cas o c just-done plan stamp-lookup out-files in-files out-op op-time dep-stamp out-stamps in-stamps missing-in missing-out all-present earliest-out latest-in up-to-date-p done-stamp (operation-done-p o c))
|#
