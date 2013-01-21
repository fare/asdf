(defpackage :hello
  (:use :cl :asdf/driver)
  (:export #:main #:entry-point))

(in-package :hello)

(defun main (&rest arguments)
  (format t "hello, world~%")
  (when arguments
    (format t "You passed ~D arguments:~%~{  ~S~%~}" (length arguments) arguments)))

(defun entry-point ()
  (apply 'main *command-line-arguments*))
