(in-package #:common-lisp-user)

(load (make-pathname :name "script-support" :defaults *load-pathname*))

(cond ((probe-file *asdf-lisp*)
       (ensure-directories-exist *asdf-fasl*)
       (multiple-value-bind (result warnings-p errors-p)
           ;; style warnings shouldn't abort the compilation [2010/02/03:rpg]
           (handler-bind ((style-warning
                           #'(lambda (w)
                               (princ w *error-output*)
                               (muffle-warning w))))
             (compile-file *asdf-lisp*
                           :output-file *asdf-fasl*))
         (declare (ignore result))
         (cond (warnings-p
                ;;; ECL gives warnings that it shouldn't!
                #+ecl (leave-lisp "ASDF compiled with warnings. Please fix ECL." 0)
                #-ecl
                (leave-lisp "Testsuite failed: ASDF compiled with warnings" 1))
               (errors-p
                (leave-lisp "Testsuite failed: ASDF compiled with ERRORS" 2))
               (t
                (leave-lisp "ASDF compiled cleanly" 0)))))
      (t
       (leave-lisp "Testsuite failed: unable to find ASDF source" 3)))
