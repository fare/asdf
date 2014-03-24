;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/syntax
  (:recycle :asdf/syntax :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/component :asdf/system)
  (:export
   #:*shared-readtable* #:*shared-pprint-dispatch*
   #:call-with-asdf-syntax #:with-asdf-syntax))
(in-package :asdf/syntax)

(with-upgradability ()
  (defvar *shared-readtable* (copy-readtable nil)
    "This shared readtable allows legacy applications to keep modifying a global shared readtable
  while maintaining some hygiene for those who want to use their own readtable.
  It is subject to the following restrictions, which always existed but were previously implicit:
  A- no modifying any standard character,
  B- no two dependencies assigning different meaning to the same non-standard character.
    Using any non-standard character while expecting the implementation to treat some way
    counts as such an assignment of meaning.
  C- libraries need to document these assignments of meaning to non-standard characters.
  D- free software libraries will register these changes on:
        http://www.cliki.net/Macro%20Characters
")
  (defvar *shared-pprint-dispatch* (copy-pprint-dispatch nil)
    "*print-pprint-dispatch* table shared by all ASDF systems.
It should match the extensions of *shared-readtable* -- see the latter variable's documentation.")

  (defun call-with-asdf-syntax (thunk &key package)
    (with-standard-io-syntax
      (let ((*readtable* *shared-readtable*)
            (*print-pprint-dispatch* *shared-pprint-dispatch*)
            (*package* (find-package (or package :asdf-user)))
            (*print-readably* nil))
        (funcall thunk))))
  (defmacro with-asdf-syntax ((&key package) &body body)
    `(call-with-asdf-syntax #'(lambda () ,@body) :package ,package)))

