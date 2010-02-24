;;; -*- Lisp -*-
;;; By Dan Weinreb, Oct 1, 2006.  Public domain, entirely free, etc.
#|
Daniel Weinreb
dlw@alum.mit.edu
http://danweinreb.org/blog/
http://ilc2009.scheming.org/
|#

(defparameter *all-tests* nil)

(defclass test ()
 ((system-name :initarg :system-name :reader test-system-name)
  (operation-name :initarg :operation-name :reader test-operation-name)
  (already-compiled :initarg :already-compiled :reader test-already-compiled)
  (expected :initarg :expected :reader test-expected)))

(defmacro define-test (test-name system-name
                       &key operation-name already-compiled expected)
 `(progn
    (push ',test-name *all-tests*)
    (setf (get ',test-name 'test)
           (make-instance 'test
             :system-name ',system-name
             :operation-name ',operation-name
             :already-compiled ',already-compiled
             :expected ',expected))))

(defclass test-file (asdf:component) ())

(defvar *test* nil)

(defvar *steps* nil)

(defmethod asdf:operation-done-p ((o asdf:compile-op) (c test-file))
 (declare (ignorable o))
 (member (asdf:component-name c) (test-already-compiled *test*)
          :test #'string=))

(defmethod asdf:operation-done-p ((o asdf:operation) (c test-file))
 (declare (ignorable o c))
 nil)

(defmethod asdf:perform ((o asdf:compile-op) (c test-file))
 (declare (ignorable o))
 (push (cons :compiled (asdf:component-name c)) *steps*))

(defmethod asdf:perform ((o asdf:load-op) (c test-file))
 (declare (ignorable o))
 (push (cons :loaded (asdf:component-name c)) *steps*))

(defun run-unit-test (test-name)
 (let ((*test* (get test-name 'test))
        (*steps* nil)
        (succeeded t))
   (flet ((fail (format-string &rest format-args)
             (setq succeeded nil)
             (format t "Error in test ~S: " test-name)
             (apply #'format t format-string format-args)
             (terpri)))
     (let ((system-name (test-system-name *test*))
            (operation-name (test-operation-name *test*)))
        (check-type system-name symbol)
        (check-type operation-name symbol)
        (asdf:operate operation-name system-name))
     (setq *steps* (nreverse *steps*))
     (loop for steps on *steps* do
        (when (member (first steps) (rest steps) :test #'equal)
          (fail "The step ~S happened more than once: ~S"
                (first steps) *steps*)))
     ;(format t "~2%STEPS: ~S~3%" *steps*)
     (dolist (expectation (test-expected *test*))
        (destructuring-bind (op file &rest at)
            expectation
          ;(format t "~2%Expectation: ~S~3%" expectation)
          (check-type file string)
          (ecase op
            ((:compiled :loaded)
             (let ((pos (position (cons op file) *steps* :test #'equal)))
               (if (null pos)
                 (fail "~S was not ~A" file op)
                 (loop for (relationship file2) on at by #'cddr do
                   (check-type file2 string)
                   (let* ((op2 (ecase relationship
                                 (:after-loading :loaded)
                                 (:after-compiling :compiled)))
                          (pos2 (position (cons op2 file2) *steps*
                                          :test #'equal)))
                     (cond ((null pos2)
                            (fail "~S was not ~A at all, after ~A was ~A"
                                  file2 op2 file op))
                           ((< pos pos2)
                            (fail "Wrong order between ~A of ~S and ~A of ~S"
                                  op file op2 file2))))))))
            (:did-not-compile
             (when (member (cons :compiled file) *steps*)
               (fail "~A compiled but should not have" file)))
            (:did-not-load
             (when (member (cons :loaded file) *steps*)
               (fail "~A loaded but should not have" file)))))))
   succeeded))

(defun run-all-unit-tests ()
 (let ((n-tests 0)
        (n-succeeded 0))
 (dolist (test-name *all-tests*)
   (incf n-tests)
   (when (run-unit-test test-name)
     (incf n-succeeded)))
 (format t "Summary: ~D of ~D tests succeeded." n-succeeded n-tests)))


(asdf:defsystem system-1
 :components ((:test-file "a")
               (:test-file "b")
               (:test-file "c" :depends-on ("b"))
               (:test-file "d")))


(define-test test-1 system-1
 :operation-name asdf:load-op
 :already-compiled ("b")
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:did-not-compile "b")
             (:loaded "b")
             (:compiled "c" :after-loading "b")
             (:loaded "c" :after-loading "b" :after-compiling "c")
             (:compiled "d")
             (:loaded "d" :after-compiling "d")))

(asdf:defsystem system-2
 :components ((:test-file "a" :depends-on ("g" "k"))
               (:test-file "b")
               (:test-file "c" :depends-on ("b"))
               (:test-file "d" :depends-on ("e"))
               (:test-file "e")
               (:test-file "f" :depends-on ("c"))
               (:test-file "g" :depends-on ("h"))
               (:test-file "h")
               (:test-file "i" :depends-on ("f" "b"))
               (:test-file "j" :depends-on ("f" "c"))
               (:test-file "k")
               (:test-file "l" :depends-on ("d"))))

(define-test test-2 system-2
 :operation-name asdf:load-op
 :already-compiled ()
 :expected ((:compiled "a" :after-compiling "g" :after-compiling "k"
                        :after-compiling "h" :after-loading "g"
                        :after-loading "k" :after-loading "h")
             (:loaded "a" :after-compiling "a")
             (:compiled "b")
             (:loaded "b" :after-compiling "b")
             (:compiled "c" :after-compiling "b" :after-loading "b")
             (:loaded "c"   :after-compiling "b" :after-loading "b"
                      :after-compiling "c")
             (:compiled "d" :after-compiling "e")
             (:compiled "e")
             (:compiled "f" :after-compiling "c" :after-compiling "b")
             (:compiled "g" :after-compiling "h")
             (:compiled "h")
             (:compiled "i" :after-compiling "f" :after-compiling "b")
             (:compiled "j" :after-compiling "f" :after-compiling "b"
                        :after-compiling "c")
             (:compiled "i" :after-compiling "f" :after-compiling "b"
                        :after-compiling "c")
             (:compiled "j" :after-compiling "f" :after-compiling "b"
                        :after-compiling "c")
             (:compiled "k")
             (:compiled "l" :after-compiling "d" :after-compiling "e")))

(asdf:defsystem system-3
 :components ((:test-file "a")
               (:test-file "b" :depends-on ("a"))))

(define-test test-3 system-3
 :operation-name asdf:compile-op
 :already-compiled ("b")
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:compiled "b" :after-compiling "a" :after-loading "a")
             (:did-not-load "b")))

(define-test test-3-a system-3
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:did-not-load "a")
             (:did-not-compile "b")
             (:did-not-load "b")))

(define-test test-3-b system-3
 :operation-name asdf:compile-op
 :already-compiled ()
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:compiled "b" :after-compiling "a" :after-loading "a")
             (:did-not-load "b")))

(asdf:defsystem system-4
 :components ((:test-file "a")
               (:test-file "b" :depends-on ("a"))))

(define-test test-4 system-4
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:did-not-load "a")
             (:did-not-compile "b")
             (:did-not-load "b")))

(asdf:defsystem system-5
 :components ((:test-file "a")
               (:test-file "b" :depends-on ("a")
                               :in-order-to ((asdf:compile-op
                                              (asdf:load-op "a"))))))

(define-test test-5 system-5
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:loaded "a")
             (:did-not-compile "b")
             (:did-not-load "b")))


(asdf:defsystem system-6
 :components ((:test-file "a")
               (:test-file "b"
                           :in-order-to ((asdf:compile-op (asdf:compile-op "a"))
                                         (asdf:load-op (asdf:load-op "a")))
                           :do-first    ((asdf:compile-op (asdf:load-op "a"))))))

(define-test test-6 system-6
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:did-not-load "a")
             (:did-not-compile "b")
             (:did-not-load "b")))

;; Should be just like test-3-b, but a does not get loaded.   Why not?
;; Because you can't really specify :do-first because it clobbers
;; the :do-first that you provide!
(define-test test-6-a system-6
 :operation-name asdf:compile-op
 :already-compiled ()
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:compiled "b" :after-compiling "a" :after-loading "a")
             (:did-not-load "b")))




(asdf:defsystem system-7
 :components ((:test-file "a")
               (:test-file "b"
                           :in-order-to ((asdf:compile-op (asdf:compile-op "a"))
                                         (asdf:load-op (asdf:load-op "a"))
                                         (asdf:compile-op (asdf:load-op "a"))))))

(define-test test-7 system-7
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:loaded "a")
             (:did-not-compile "b")
             (:did-not-load "b")))
