;;;; -------------------------------------------------------------------------
;;;; Lisp Evaluation

(uiop/package:define-package :uiop/eval
  (:recycle :uiop/eval :asdf)
  (:use :uiop/common-lisp :uiop/utility :uiop/stream)
  (:export
   #:*standard-readtable* #:*standard-print-pprint-dispatch*
   #:*shared-readtable* #:*shared-print-pprint-dispatch*
   #:*standard-syntax-variables*
   #:call-with-shared-readtable #:call-with-standard-io-syntax
   #:eval-input #:eval-thunk #:standard-eval-thunk
   #:ensure-variable #:variable-value))
(in-package :uiop/eval)

;;; Safe syntax
(with-upgradability ()
  (defvar *standard-readtable* (with-standard-io-syntax *readtable*)
    "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

  (defvar *standard-print-pprint-dispatch* (with-standard-io-syntax *print-pprint-dispatch*)
    "The standard pprint dispatch table, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

  (defvar *shared-readtable* *readtable*
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
  (defvar *shared-print-pprint-dispatch* *print-pprint-dispatch*
    "*print-pprint-dispatch* table shared by all ASDF systems.
It should match the extensions of *shared-readtable* -- see the latter variable's documentation.")

  (defvar *standard-syntax-variables*
    `((*package* . (find-package :cl-user))
      (*print-array* . t)
      (*print-base* . 10)
      (*print-case* . :upcase)
      (*print-circle* . nil)
      (*print-escape* . t)
      (*print-gensym* . t)
      (*print-length* . nil)
      (*print-level* . nil)
      (*print-lines* . nil)
      (*print-miser-width* . nil)
      (*print-pprint-dispatch* . ,*standard-print-pprint-dispatch*)
      (*print-pretty* . nil)
      (*print-radix* . nil)
      (*print-readably* . t)
      (*print-right-margin* . nil)
      (*read-base* . 10)
      (*read-default-float-format* . single-float)
      (*read-eval* . t)
      (*read-suppress* . nil)
      (*readtable* . ,*standard-readtable*))))

(with-upgradability ()
  (defun call-with-standard-io-syntax (function)
    (with-standard-io-syntax (call-function function)))

  (defun call-with-shared-readtable (thunk)
    (let ((*readtable* *shared-readtable*)) (call-function thunk)))

  (defun eval-input (input)
    "Portably read and evaluate forms from INPUT, return the last values."
    (with-input (input)
      (loop :with results :with eof ='#:eof
            :for form = (read input nil eof)
            :until (eq form eof)
            :do (setf results (multiple-value-list (eval form)))
            :finally (return (apply 'values results)))))

  (defun eval-thunk (thunk)
    "Evaluate a THUNK of code:
If a function, FUNCALL it without arguments.
If a constant literal and not a sequence, return it.
If a cons or a symbol, EVAL it.
If a string, repeatedly read and evaluate from it, returning the last values."
    (etypecase thunk
      ((or boolean keyword number character pathname) thunk)
      ((or cons symbol) (eval thunk))
      (function (funcall thunk))
      (string (eval-input thunk))))

  (defun standard-eval-thunk (thunk &key (package :cl))
    "Like EVAL-THUNK, but in a more standardized evaluation context."
    ;; Note: it's "standard-" not "safe-", because evaluation is never safe.
    (when thunk
      (with-safe-io-syntax (:package package)
        (let ((*read-eval* t))
          (eval-thunk thunk))))))

;;; Late-binding variables
(with-upgradability ()
  (defun ensure-variable (name &key package (when-undefined 'error))
    (etypecase name
      (symbol name)
      (string (or (ignore-errors
                   (let ((s (safe-read-from-string name :package package)))
                     (and (symbolp s) s)))
                  (call-function when-undefined
                                 "Cannot read non-nil symbol from ~S" name)))))

  (defun variable-value (name &key package (when-undefined 'error))
    (let ((var (ensure-variable name :package package :when-undefined when-undefined)))
      (if (boundp var)
          (symbol-value var)
          (call-function when-undefined "Symbol ~S unbound" name))))

  (defun (setf variable-value) (value name &key package (when-undefined 'error))
    (if-let (var (ensure-variable name :package package :when-undefined when-undefined))
      (setf (symbol-value var) value)
      value)))
