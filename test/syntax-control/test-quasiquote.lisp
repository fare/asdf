(in-package :cl-user)

(defparameter *quine*
  '((LAMBDA (X) (LIST X `',X)) '(LAMBDA (X) (LIST X `',X))))

;; Interestingly, at least on SBCL, this assertion does not hold:
#-(or sbcl) (assert (equal *quine* (eval *quine*)))

(assert (equal (write-to-string *quine*)
               (write-to-string (eval *quine*))))

(assert (equal (eval *quine*)
               (eval (eval *quine*))))

