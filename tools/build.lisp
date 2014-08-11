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
    (flet ((lisp-only (x) (remove "lisp" x :test-not 'equal :key 'pathname-type)))
      (let ((driver-files (mapcar #'(lambda (x) (subpathname "uiop/" x)) (lisp-only (uiop-files))))
            (defsystem-files (lisp-only (asdf-defsystem-files))))
        (run `(pipe (wc ,@driver-files) (sort -n)))
        (terpri)
        (run `(pipe (wc header.lisp ,@defsystem-files) (sort -n)))
        (terpri)
        (run `(pipe (wc header.lisp ,@driver-files ,@defsystem-files) (tail -n 1))))))
  (values))
