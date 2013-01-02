(cl:in-package :common-lisp-user)

(defun load-pathname ()
  #-gcl *load-pathname*
  #+gcl ;; Debian's GCL 2.7 has bugs with compiling multiple-value stuff, but can run ASDF 2.011
  (symbol-value
   (find-symbol
    "*LOAD-PATHNAME*"
    (if (or (< system::*gcl-major-version* 2) ;; GCL 2.6 fails to fully compile ASDF at all
            (and (= system::*gcl-major-version* 2)
                 (< system::*gcl-minor-version* 7)))
        :system :cl))))

(load (make-pathname :name "script-support" :type "lisp" :defaults (load-pathname))
      #+gcl :print #+gcl t)

(in-package :asdf-test)

(declaim (optimize (speed 2) (safety 3) #-(or allegro gcl) (debug 3)
		   #+(or cmu scl) (c::brevity 2)))
(proclaim '(optimize (speed 2) (safety 3) #-(or allegro gcl) (debug 3)
		     #+(or cmu scl) (c::brevity 2)))

(cond
  ((not (probe-file *asdf-lisp*))
   (leave-lisp "Testsuite failed: unable to find ASDF source" 3))
  ((and (probe-file *asdf-fasl*)
        (> (file-write-date *asdf-fasl*) (file-write-date *asdf-lisp*))
        (ignore-errors (load *asdf-fasl*)))
   (leave-lisp "Reusing previously-compiled ASDF" 0))
  (t
   (load-asdf-lisp)
   (let ((tmp (make-pathname :name "asdf-tmp" :defaults *asdf-fasl*)))
     (multiple-value-bind (result warnings-p errors-p)
         (compile-asdf tmp)
       (declare (ignore result))
       (cond
         (errors-p
          (leave-lisp "Testsuite failed: ASDF compiled with ERRORS" 2))
         #-(or cmu ecl scl xcl)
	 ;; ECL 11.1.1 has spurious warnings, same with XCL 0.0.0.291.
         ;; SCL has no warning but still raises the warningp flag since 2.20.15 (?)
         (warnings-p
          (leave-lisp "Testsuite failed: ASDF compiled with warnings" 1))
         (t
          (when warnings-p
            (format t "Your implementation raised warnings, but they were ignored~%"))
          (when (probe-file *asdf-fasl*)
            (delete-file *asdf-fasl*))
          (rename-file tmp *asdf-fasl*)
          (leave-lisp "ASDF compiled cleanly" 0)))))))
