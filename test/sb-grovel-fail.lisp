(in-package #:cl-user)

(defun this-should-be-after-the-defstruct (example)
  (example-slot example))

(defstruct example
  slot)
