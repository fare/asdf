(in-package :asdf-tools)

(defun call-with-all-lisps (thunk &optional (lisps *test-lisps*))
  (apply 'all-pass
         (loop :for lisp :in lisps
               :collect (let ((*test-lisps* (list lisp))) (funcall thunk)))))

(defmacro with-all-lisps ((&rest maybe-lisps) &body body)
  `(call-with-all-lisps (lambda () ,@body) ,@maybe-lisps))

(deftestcmd test-all-clean-load ()
  "test that all lisp implementations can load asdf cleanly without any output message"
  (with-all-lisps () (test-clean-load)))

(deftestcmd test-all-lisp ()
  "test that all lisp implementations pass all asdf test scripts"
  (with-all-lisps () (test-scripts)))

(deftestcmd test-all-no-upgrade ()
  "test that all lisp implementations pass all normal asdf; also test-basic"
  (all-pass (test-basic) (test-all-lisp)))

(deftestcmd test-all-upgrade ()
  "test that all lisp implementations pass all asdf upgrade tests"
  (with-all-lisps (*upgrade-test-lisps*) (test-upgrade)))

(deftestcmd test-all-scripts ()
  "test that all lisp implementations pass all asdf tests"
  (all-pass (test-all-no-upgrade) (test-all-upgrade)))
(defalias test-all test-all-scripts)

(deftestcmd test-all-scripts-no-stop ()
  "test that all lisp implementations pass all asdf test scripts, but don't stop on error"
  (with-all-lisps ()
    (ignore-errors (test-scripts))))

(deftestcmd test-all-upgrade-no-stop ()
  "test that all lisp implementations pass all asdf upgrade tests, but don't stop on error"
  (with-all-lisps (*upgrade-test-lisps*)
    (ignore-errors (test-upgrade))))

(deftestcmd test-all-no-upgrade-no-stop ()
  "test that all lisp implementations pass all normal asdf tests (no upgrade), but don't stop on error."
  (all-pass
   (test-basic)
   (test-all-clean-load)
   (test-all-scripts-no-stop)
   (check-all-scripts-results)))

(deftestcmd test-all-no-stop () ;; TODO: pass arguments!
  "test that all lisp implementations pass all asdf tests (including upgrade), but don't stop on error."
  (all-pass
   (test-basic)
   (test-all-clean-load)
   (test-all-scripts-no-stop)
   (test-all-upgrade-no-stop)
   (check-all-results)))

(deftestcmd check-all-scripts-results ()
  "were there errors in test scripts?"
  (with-asdf-dir ()
    (let ((a (run/lines
              `(grep "-L" "[5-9][0-9] passing and 0 failing"
                     ,(mapcar (lambda (l) (format nil "build/results/~(~A~)-test.text" l))
                              *test-lisps*)))))
      (or (null a)
          (progn
            (format! *error-output* "Unexpected test failures on these implementations:~%~{~A~%~}" a)
            nil)))))

(deftestcmd check-all-upgrade-results ()
  "were there upgrade tests failures?"
  (with-asdf-dir ()
    (let ((a (run/lines
              `(grep "-L" "Upgrade test succeeded for "
                     ,(mapcar (lambda (l) (format nil "build/results/~(~A~)-upgrade.text" l))
                              *upgrade-test-lisps*)))))
      (or (null a)
          (progn
            (format t "Unexpected upgrade failures on these implementations:~%~{~A~%~}~%" a)
            nil)))))

(deftestcmd check-all-results ()
  "check that there were no errors in either test scripts or upgrade tests"
  (all-pass (check-all-scripts-results) (check-all-upgrade-results)))

