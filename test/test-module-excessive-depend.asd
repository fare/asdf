(defpackage :tmed-asd
    (:use #:asdf :common-lisp))

(in-package :tmed-asd)

(defsystem :test-module-excessive-depend
    :components ((:file "file1")
                 (:module "quux"
                          :pathname #p""
                          :depends-on ("file1")
                          :components ((:file "file2")))))

(defun find-quux ()
  (find-component
   (find-system :test-module-excessive-depend)
   "quux"))

(defun find-file2 ()
  (find-component (find-quux) "file2"))

(defmethod component-depends-on ((op load-op)
                                 (c (eql (find-file2))))
  (cons (cons 'load-op (list "file3-only"))
        (call-next-method)))

(defmethod component-depends-on ((op compile-op)
                                 (c (eql (find-file2))))
  (cons (cons 'load-op (list "file3-only"))
        (call-next-method)))

(defmethod find-component :around ((m (eql (find-quux)))
                                   (c string))
  "FIND-COMPONENT on a component is a no-op --- it's already found."
  (declare (ignore version))
  (if (string-equal c "file3-only")
      (asdf:find-system c)
      (call-next-method)))
