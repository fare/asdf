;;;; ---------------------------------------------------------------------------
;;;; ASDF-USER, where the action happens.

(uiop/package:define-package :asdf/user
  (:nicknames :asdf-user)
  ;; TODO: it would be nice to have :UIOP in the list,
  ;; but we need test compatibility with cl-test-grid first.
  (:use :uiop/common-lisp :uiop/package :asdf/interface))
