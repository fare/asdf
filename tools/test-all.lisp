(in-package :asdf-tools)

(defun call-with-all-lisps (thunk &optional (lisps *test-lisps*))
  (apply 'all-pass
         (loop :for lisp :in lisps
               :collect (let ((*test-lisps* (list lisp))) (funcall thunk)))))

(defmacro with-all-lisps ((&rest maybe-lisps) &body body)
  `(call-with-all-lisps (lambda () ,@body) ,@maybe-lisps))

(deftestcmd test-all-clean-load ()
  "test-clean-load on all lisps"
  (with-all-lisps () (test-clean-load)))

(deftestcmd test-all-scripts ()
  "test-scripts on all lisps"
  (with-all-lisps () (test-scripts)))

(deftestcmd test-all-no-upgrade ()
  "test-basic, and test-all-script"
  (all-pass (test-basic) (test-all-scripts)))

(deftestcmd test-all-upgrade ()
  "test-upgrade on all lisps"
  (with-all-lisps (*upgrade-test-lisps*) (test-upgrade)))

(deftestcmd test-all ()
  "all tests"
  (all-pass (test-all-no-upgrade) (test-all-upgrade)))

(deftestcmd test-all-scripts-no-stop ()
  "test-scripts on all lisps, no stop"
  (with-all-lisps ()
    (ignore-errors (test-scripts))))

(deftestcmd test-all-upgrade-no-stop ()
  "test-upgrade on all lisps, no stop"
  (with-all-lisps (*upgrade-test-lisps*)
    (ignore-errors (test-upgrade))))

(deftestcmd test-all-no-upgrade-no-stop ()
  "all tests but upgrade on all lisps, no stop"
  (all-pass
   (doc) (test-load-systems)
   (test-all-clean-load)
   (test-all-scripts-no-stop)
   (check-all-scripts-results)))

(deftestcmd test-all-no-stop () ;; TODO: pass arguments!
  "all tests on all lisps, no stop"
  (all-pass
   (doc) (test-load-systems)
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
  "were there upgrade test failures?"
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
  "were there failures in any scripts or upgrade?"
  (all-pass (check-all-scripts-results) (check-all-upgrade-results)))

