(in-package :asdf-tools)

(defun build-asdf ()
  "Make sure asdf.lisp is built"
  (load-system :asdf)
  (values))

;;; Documentation
(defun doc ()
  (run '(make) :directory (pn "doc/")))

(defun website ()
  (run '(make website) :directory (pn "doc/")))


;;; Line counting
(defun wc ()
  (with-asdf-dir ()
    (run `(pipe (wc ,@(driver-files)) (sort -n)))
    (terpri)
    (run `(pipe (wc header.lisp ,@(asdf-defsystem-files)) (sort -n)))
    (terpri)
    (run `(pipe (wc header.lisp ,@(driver-files) ,@(asdf-defsystem-files)) (tail -n 1)))))

