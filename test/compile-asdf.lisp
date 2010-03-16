(in-package #:common-lisp-user)

(load (make-pathname :name "script-support" :defaults *load-pathname*))

(cond
  ((not (probe-file *asdf-lisp*))
   (leave-lisp "Testsuite failed: unable to find ASDF source" 3))
  ((and (probe-file *asdf-fasl*)
        (> (file-write-date *asdf-fasl*) (file-write-date *asdf-lisp*)))
   (leave-lisp "Reusing previously-compiled ASDF" 0))
  (t
   (let ((tmp (make-pathname :name "asdf-tmp" :defaults *asdf-fasl*)))
     (ensure-directories-exist *asdf-fasl*)
     (multiple-value-bind (result warnings-p errors-p)
         ;; style warnings shouldn't abort the compilation [2010/02/03:rpg]
         (handler-bind ((style-warning
                         #'(lambda (w)
                             (princ w *error-output*)
                             (muffle-warning w))))
           (compile-file *asdf-lisp* :output-file tmp))
       (declare (ignore result))
       (cond
         #-ecl
         (warnings-p
          (leave-lisp "Testsuite failed: ASDF compiled with warnings" 1))
         (errors-p
          (leave-lisp "Testsuite failed: ASDF compiled with ERRORS" 2))
         (t
          #+ecl
          (when warnings-p
            (format t "~&ASDF compiled with warnings. Please fix ECL.~%"))
          (ignore-errors (delete-file *asdf-fasl*))
          (rename-file tmp *asdf-fasl*)
          (leave-lisp "ASDF compiled cleanly" 0)))))))
