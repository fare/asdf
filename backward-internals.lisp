;;;; -------------------------------------------------------------------------
;;; Internal hacks for backward-compatibility 

(asdf/package:define-package :asdf/backward-internals
  (:recycle :asdf/backward-internals :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade
   :asdf/system :asdf/component :asdf/find-system :asdf/action)
  (:export ;; for internal use
   #:%refresh-component-inline-methods
   #:%resolve-if-component-dep-fails))
(in-package :asdf/backward-internals)

;;;; Backward compatibility with "inline methods"

(defparameter +asdf-methods+
  '(perform-with-restarts perform explain output-files operation-done-p))

(defun* %remove-component-inline-methods (component)
  (dolist (name +asdf-methods+)
    (map ()
         ;; this is inefficient as most of the stored
         ;; methods will not be for this particular gf
         ;; But this is hardly performance-critical
         #'(lambda (m)
             (remove-method (symbol-function name) m))
         (component-inline-methods component)))
  ;; clear methods, then add the new ones
  (component-inline-methods component) nil)

(defun* %define-component-inline-methods (ret rest)
  (dolist (name +asdf-methods+)
    (let ((keyword (intern (symbol-name name) :keyword)))
      (loop :for data = rest :then (cddr data)
        :for key = (first data)
        :for value = (second data)
        :while data
        :when (eq key keyword) :do
        (destructuring-bind (op qual (o c) &body body) value
          (pushnew
           (eval `(defmethod ,name ,qual ((,o ,op) (,c (eql ,ret)))
                             ,@body))
           (component-inline-methods ret)))))))

(defun* %refresh-component-inline-methods (component rest)
  (%remove-component-inline-methods component)
  (%define-component-inline-methods component rest))

;;;; PARTIAL SUPPORT for the :if-component-dep-fails component attribute
;; and the companion asdf:feature pseudo-dependency.
;; This won't recurse into dependencies to accumulate feature conditions.
;; Therefore it will accept the SB-ROTATE-BYTE of an old SBCL
;; (older than 1.1.2.20-fe6da9f) but won't suffice to load an old nibbles.
(defun* %resolve-if-component-dep-fails (if-component-dep-fails component)
  (asdf-message "The system definition for ~S uses deprecated ~
                 ASDF option :IF-COMPONENT-DEP-DAILS. ~
                 Starting with ASDF 2.27, please use :IF-FEATURE instead"
                (coerce-name (component-system component)))
  ;; This only supports the pattern of use of the "feature" seen in the wild
  (check-type component parent-component)
  (check-type if-component-dep-fails (member :fail :ignore :try-next))
  (unless (eq if-component-dep-fails :fail)
    (loop :with o = (make-instance 'compile-op)
      :for c :in (component-children component) :do
        (loop :for (feature? feature) :in (component-depends-on o c)
              :when (eq feature? 'feature) :do
                (setf (component-if-feature c) feature)))))

