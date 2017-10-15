;;;; -------------------------------------------------------------------------
;;;; Lisp Evaluation

(uiop/package:define-package :uiop/eval
  (:recycle :uiop/eval :asdf)
  (:use :uiop/common-lisp :uiop/utility :uiop/stream)
  (:export
   #:*initial-readtable* #:*initial-print-pprint-dispatch*
   #:*standard-readtable* #:*standard-print-pprint-dispatch*
   #:*shared-readtable* #:*shared-print-pprint-dispatch*
   #:*standard-syntax-variables*
   #:call-with-shared-syntax #:with-shared-syntax
   #:eval-input #:eval-thunk #:shared-eval-thunk
   #:ensure-variable #:variable-value))
(in-package :uiop/eval)

;;; Safe syntax
(with-upgradability ()
  (defvar *initial-readtable* *readtable*
    "The initial readtable, as inherited from the environment at the time UIOP was first loaded.
It initially implements the syntax specified by the CLHS, but may also have implementation-specific
extensions, and may grow user-defined extensions. You MUST ONLY make conservative extensions to it:
  A- No one may at any point modify any standard character in any way.
  B- No two dependencies may assign different meaning to the same non-standard character.
    Using any non-standard character while expecting the implementation to treat some way
    counts as such an assignment of meaning (e.g. using CCL's #$ syntax for FFI).
  C- Libraries need to document these assignments of meaning to non-standard characters.
  D- Free software libraries will register these changes on:
        http://www.cliki.net/Macro%20Characters
The above restrictions have always existed but were previously implicit.
There is unhappily no enforcement against non-conservative extensions, so tread carefully.
Do not re-bind this variable.")

  (defvar *initial-print-pprint-dispatch* (with-standard-io-syntax *print-pprint-dispatch*)
    "The initial readtable, as inherited from the environment at the time UIOP was first loaded.
It initially implements the syntax specified by the CLHS, but may also have implementation-specific
extensions, and may grow user-defined extensions. You MUST ONLY make conservative extensions to it,
i.e. you may not modify standard behavior, or conflict with anyone else's previous extensions.
There is unhappily no enforcement against non-conservative extensions, so tread carefully.
Do not re-bind this variable.")

  (defvar *standard-readtable* (with-standard-io-syntax *readtable*)
    "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified in any way whatsoever.
However, only SBCL is known to enforce this prohibition.
Do not re-bind this variable.")

  (defvar *standard-print-pprint-dispatch* (with-standard-io-syntax *print-pprint-dispatch*)
    "The standard pprint dispatch table, implementing the syntax specified by the CLHS.
It must never be modified in any way whatsoever.
However, only SBCL is known to enforce this prohibition.
Do not re-bind this variable.")

  (defvar *shared-readtable* *initial-readtable*
    "The shared readtable used by all Lisp software while building with ASDF.
This shared readtable is usually the *initial-readtable*, in which case
conservative readtable extensions are permitted and such extensions only;
see the documentation of that variable for more details.
For even stricter enforcement against modifications at all, this readtable may be bound to
the *standard-readtable* (at least on SBCL); this helps enforce good behavior from all libraries,
but is not compatible with some older libraries.
This readtable can also be bound to other readtables to implement various instrumentations
by Lisp-to-Lisp compilers when (re)building software, presumably with a different fasl cache.")
  (defvar *shared-print-pprint-dispatch* *initial-print-pprint-dispatch*
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

;;;; Syntax control
(with-upgradability ()
  (defun call-with-shared-syntax (function &key package readably)
    (with-standard-io-syntax
      (let ((*readtable* *shared-readtable*)
            (*print-pprint-dispatch* *shared-print-pprint-dispatch*)
            (*package* (find-package (or package :cl-user)))
            (*print-readably* readably))
        (call-function function))))

  (defmacro with-shared-syntax ((&rest keys &key &allow-other-keys) &body body)
    `(call-with-shared-syntax #'(lambda () ,@body) ,@keys))

  (defun eval-input (input)
    "Portably read and evaluate forms from INPUT, return the last values."
    (with-input (input)
      (loop :with results :with eof = '#:eof
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

  (defun shared-eval-thunk (thunk &key (package :cl))
    "Like EVAL-THUNK, but in the shared evaluation context."
    ;; Note: it's "shared-" not "safe-", because evaluation is never safe.
    (when thunk
      (with-shared-syntax (:package package)
        (eval-thunk thunk)))))

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
