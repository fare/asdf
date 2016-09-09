;;;
;;; DESCRIPTION:
;;;
;;; This file builds a standalone executable with a dependency on
;;; ASDF.
;;;
;;;
;;; USE:
;;;
;;; Launch a copy of ECL and load this file in it
;;;
;;;	(load "readme.lisp")
;;;
(require 'asdf)

(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILDING A STANDALONE EXECUTABLE
;;;
")

;;
;; * Combine files in a standalone executable. We reuse the files
;;   from the previous example
;;

(defconstant +standalone-exe+ (compile-file-pathname "hellow" :type :program))

(push (make-pathname :name nil :type nil :version nil
                     :defaults *load-truename*)
      asdf:*central-registry*)

(asdf:make-build :hellow
                 :type :program
                 :move-here "./"
                 :prologue-code "printf(\"Good morning sunshine!\");"
                 :epilogue-code '(progn
                                  (format t "~%Good bye sunshine.~%")
                                  (ext:quit 0)))

;;
;; * Test the program
;;
(format t "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TESTING A STANDALONE EXECUTABLE
;;;

")
(uiop:run-program (format nil "./~A" +standalone-exe+) :output *standard-output*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLEAN UP
;;;

(delete-file +standalone-exe+)
