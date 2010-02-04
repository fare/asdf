(in-package #:common-lisp-user)

(load "test/script-support.lisp")

(cond ((probe-file "asdf.lisp")
       (multiple-value-bind (result warnings-p errors-p)
           ;; style warnings shouldn't abort the compilation [2010/02/03:rpg]
           (handler-bind ((style-warning
                           #'(lambda (w)
                               (princ w *error-output*)
                               (muffle-warning w))))
             (compile-file "asdf.lisp"))
         (declare (ignore result))
         (cond (warnings-p
                #-ecl ;;; ECL gives warnings that it shouldn't!
                (leave-lisp "Testsuite failed: ASDF compiled with warnings" 1))
               (errors-p
                (leave-lisp "Testsuite failed: ASDF compiled with ERRORS" 2))
               (t
                (leave-lisp "ASDF compiled cleanly" 0)))))
      (t
       (leave-lisp "Testsuite failed: unable to find ASDF source" 3)))
