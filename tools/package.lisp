(defpackage :asdf-tools
  (:use :common-lisp :uiop :asdf
   :fare-utils :inferior-shell :lisp-invocation :lisp-invocation/non-special :cl-ppcre :optima :optima.ppcre)
  (:export ;; TODO: export stuff
   ;; failure
   #:with-failure-context #:success #:failure #:success-if #:failure-if #:fail!
   #:without-stopping #:call-without-stopping #:run-command))

;; Just so we can use the name in our test scripts...
(defpackage :asdf-test
  (:use :common-lisp))
