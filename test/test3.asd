;;; -*- Lisp -*-
(asdf:defsystem test3
    :components
  ((:module "deps"
	    :if-component-dep-fails :try-next
	    :pathname "."
	    :components
	    ((:file "file1" :in-order-to ((compile-op (feature :f1))))
	     (:file "file2" :in-order-to ((compile-op (feature :f2))))))))

