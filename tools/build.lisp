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

