(in-package :asdf-tools)

(defun call-with-all-lisps (thunk &optional (lisps *test-lisps*))
  (apply 'all-pass
         (loop :for lisp :in (get-lisps lisps)
               :collect (funcall thunk lisp))))

(defmacro with-all-lisps ((&optional (lisp-var 'lisp) (lisps '*test-lisps*)) &body body)
  `(call-with-all-lisps (lambda (,lisp-var) ,@body) ,lisps))

(deftestcmd test-all-clean-load (lisps)
  "test-clean-load on all lisps"
  (with-all-lisps (l lisps) (test-clean-load l)))

(deftestcmd test-all-scripts (lisps)
  "test-scripts on all lisps"
  (with-all-lisps (l lisps) (test-scripts l)))

(deftestcmd test-all-no-upgrade ()
  "test-basic, and test-all-script"
  (all-pass (test-basic) (test-all-scripts)))

(deftestcmd test-all-upgrade (upgrade-lisps)
  "test-upgrade on all lisps"
  (with-all-lisps (l upgrade-lisps) (test-upgrade l)))

(deftestcmd test-all ()
  "all tests"
  (all-pass (test-all-no-upgrade) (test-all-upgrade)))

(deftestcmd test-all-scripts-no-stop (lisps)
  "test-scripts on all lisps, no stop"
  (with-all-lisps (l lisps) (ignore-errors (test-scripts l))))

(deftestcmd test-all-upgrade-no-stop (upgrade-lisps)
  "test-upgrade on all lisps, no stop"
  (with-all-lisps (l upgrade-lisps) (ignore-errors (test-upgrade l))))

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
      (if (null a) (success)
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
      (if (null a) (success)
          (progn
            (format t "Unexpected upgrade failures on these implementations:~%~{~A~%~}~%" a)
            nil)))))

(deftestcmd check-all-results ()
  "were there failures in any scripts or upgrade?"
  (all-pass (check-all-scripts-results) (check-all-upgrade-results)))

