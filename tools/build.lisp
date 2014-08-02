(in-package :asdf-tools)

(defun build-asdf ()
  "make sure asdf.lisp is built"
  (load-system :asdf)
  (values))

;;; Documentation
(defun doc ()
  "build documentation in doc/ directory"
  (run '(make) :directory (pn "doc/")))

(defun website ()
  "publish documentation onto the public website"
  (run '(make website) :directory (pn "doc/")))


;;; Line counting
(defun wc ()
  "count lines of lisp code in asdf and uiop"
  (with-asdf-dir ()
    (run `(pipe (wc ,@(driver-files)) (sort -n)))
    (terpri)
    (run `(pipe (wc header.lisp ,@(asdf-defsystem-files)) (sort -n)))
    (terpri)
    (run `(pipe (wc header.lisp ,@(driver-files) ,@(asdf-defsystem-files)) (tail -n 1)))))


;;; BONUS: install asdf as module for your favorite Lisp implementation.
(deftestcmd install-asdf (lisp)
  "install asdf as a module on specified Lisp"
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

