(in-package :asdf-tools)

(deftestcmd %load (lisp) ;; load would be a clash, so use %load instead
  "make load will start a Lisp and load ASDF from individual source files.
This is great to quickly locate compilation errors and interactively debug ASDF."
  (with-asdf-dir ()
    (run-test-lisp
     (format nil "loading ASDF into an interactive ~(~A~)" lisp)
     `((load "test/script-support.lisp")
       (asdf-test::interactive-test
        ',(system-source-files :asdf/defsystem :monolithic t)))
     :lisp lisp :debugger t :output :interactive)))

(deftestcmd test-load-systems (lisp systems)
  "test that the preferred lisp implementation can load your favorite systems without error"
  (with-asdf-dir ()
    (let* ((log (newlogfile "systems" lisp)))
      (log! log "Loading all these systems on ~(~A~):~{~%  ~A~}~%~%" lisp systems)
      (run-test-lisp
       "loading the systems"
       `((load "test/script-support.lisp")
         (asdf-test::with-test () (asdf-test::test-load-systems ,@systems)))
       :lisp lisp :log log))))

(deftestcmd test-clean-load (lisp log)
  "test that the preferred lisp implementation can load asdf cleanly without any output message"
  (nest
   (block ()
     (case lisp ((:gcl :cmucl) (return t)))) ;; These are hopeless
   (with-asdf-dir ())
   (let ((nop (newlogfile "nop" lisp))
         (load (newlogfile "load" lisp)))
     (run-test-lisp
      (format nil "Starting ~(~A~), loading the script support, and exiting without doing anything" lisp)
      `((load "test/script-support.lisp" :verbose nil :print nil)
        (asdf-test::exit-lisp 0))
      :lisp lisp :output nop :log log)
     (run-test-lisp
      (format nil "Starting ~(~A~), loading the script support, loading ASDF from source, then exiting" lisp)
      `((load "test/script-support.lisp" :verbose nil :print nil)
        (load "build/asdf.lisp" :verbose nil :print nil)
        "(uiop/image:quit 0)")
      :lisp lisp :output load :log log)
     (if (nth-value 2 (run `(diff ,nop ,load)
                           :output :interactive :error-output :output :input nil))
         (progn
           (log! log "GOOD: Loading ASDF on ~(~A~) produces no message" lisp)
           (return t))
         (progn
           (log! log "BAD: Loading ASDF on ~(~A~) produces messages" lisp)
           (return nil))))))

;;; BONUS: install asdf as module for your favorite Lisp implementation.
(deftestcmd install-asdf (lisp)
  (flet ((doit ()
           (with-asdf-dir ()
             (run-test-lisp
              (format nil "installing ASDF to be provided as a module on ~(~A~)" lisp)
              '((load "tools/install-asdf.lisp")(uiop:quit))
              :lisp lisp))))
    (case lisp
      ((:allegro :allegromodern :ccl :clisp :cmucl :lispworks :sbcl :scl :xcl)
       (doit))
      ((:abcl :ecl :ecl_bytecodes :mkcl)
       (format t "Upgrading the implementation-provided ASDF on ~(~A~) isn't supported (yet).
Happily, that implementation is known to keep ASDF reasonably up to date.~%" lisp))
      ((:cormancl :gcl :genera :mcl :mocl)
       (format t "Installing ASDF so it is provided by ~(~A~) isn't supported.
If you care, go hack the implementation.~%" lisp))
      (otherwise
       (if (string-prefix-p "allegro" (string-downcase lisp))
           (doit)
           (error "Unknown implementation ~(~A~)" lisp))))))

(deftestcmd test-basic (lisp systems)
  "basic smoke test"
  (doc)
  (test-clean-load lisp)
  (test-load-systems lisp systems))
