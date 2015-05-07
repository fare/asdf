(in-package :asdf-tools)

(defun call-with-all-lisps (thunk &key (lisps *test-lisps*) (fail-fast t))
  (let ((thunks
          (loop :for lisp :in (get-lisps lisps)
                :collect (lambda () (with-failure-context ((format nil "using ~(~A~)" lisp))
                                      (funcall thunk lisp))))))
    (if fail-fast
        (progn (map () 'funcall thunks) (success))
        (apply 'call-without-stopping thunks))))

(defmacro with-all-lisps ((lisp-var lisps &key (fail-fast t)) &body body)
  `(call-with-all-lisps (lambda (,lisp-var) ,@body) :lisps ,lisps :fail-fast ,fail-fast))

(deftestcmd test-all-clean-load (lisps)
  "test-clean-load on all lisps"
  (with-all-lisps (l lisps) (test-clean-load l)))

(deftestcmd test-all-scripts (lisps)
  "test-scripts on all lisps"
  (with-all-lisps (l lisps) (test-scripts l)))

(deftestcmd test-all-no-upgrade ()
  "test-basic, and test-all-script"
  (test-basic) (test-all-scripts))

(deftestcmd test-all-upgrade (upgrade-lisps)
  "test-upgrade on all lisps"
  (with-all-lisps (l upgrade-lisps) (test-upgrade l)))

(deftestcmd test-all ()
  "all tests"
  (test-all-no-upgrade) (test-all-upgrade))

(deftestcmd test-all-scripts-no-stop (lisps)
  "test-scripts on all lisps, no stop"
  (with-all-lisps (l lisps :fail-fast nil) (test-scripts l)))

(deftestcmd test-all-upgrade-no-stop (upgrade-lisps)
  "test-upgrade on all lisps, no stop"
  (with-all-lisps (l upgrade-lisps :fail-fast nil) (test-upgrade l)))

(deftestcmd test-all-no-upgrade-no-stop ()
  "all tests but upgrade on all lisps, no stop"
  (without-stopping ()
    (doc) (test-load-systems)
    (test-all-clean-load)
    (test-all-scripts-no-stop)
    (check-all-scripts-results)))

(deftestcmd test-all-no-stop () ;; TODO: pass arguments!
  "all tests on all lisps, no stop"
  (without-stopping ()
    (doc) (test-load-systems)
    (test-all-clean-load)
    (test-all-scripts-no-stop)
    (test-all-upgrade-no-stop)
    (check-all-results)))

(deftestcmd check-all-scripts-results ()
  "were there errors in test scripts?"
  (with-asdf-dir ()
    (let ((bad-lisps
            (run/lines
             `(grep "-L" "[5-9][0-9] passing and 0 failing"
                    ,@(mapcar (lambda (l) (format nil "build/results/~(~A~)-test.text" l))
                              *test-lisps*)))))
    (failure-if bad-lisps
                "Unexpected test failures on these implementations:~%~{~A~%~}" bad-lisps))))

(deftestcmd check-all-upgrade-results ()
  "were there upgrade test failures?"
  (with-asdf-dir ()
    (let ((bad-lisps
            (run/lines
             `(grep "-L" "Upgrade test succeeded for "
                    ,@(mapcar (lambda (l) (format nil "build/results/~(~A~)-upgrade.text" l))
                              *upgrade-test-lisps*)))))
      (failure-if bad-lisps
                  "Unexpected upgrade failures on these implementations:~%~{~A~%~}~%" bad-lisps))))

(deftestcmd check-all-results ()
  "were there failures in any scripts or upgrade?"
  (without-stopping ()
    (check-all-scripts-results) (check-all-upgrade-results)))
