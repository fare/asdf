;;;; -------------------------------------------------------------------------
;;;; Components

(asdf/package:define-package :asdf/component
  (:recycle :asdf/component :asdf)
  (:fmakunbound #:component-relative-pathname #:source-file-type)
  (:use :common-lisp :asdf/implementation :asdf/utility :asdf/pathname :asdf/upgrade)
  (:intern #:documentation #:long-documentation)
  (:export
   #:component #:component-find-path
   #:component-name #:component-pathname #:component-relative-pathname
   #:component-parent #:component-system #:component-parent-pathname
   #:source-file-type ;; backward-compatibility
   #:component-in-order-to #:component-sibling-dependencies
   #:component-if-feature #:around-compile-hook
   #:component-description #:component-long-description
   #:component-version #:version-satisfies
   #:component-properties #:component-property ;; backward-compatibility only. DO NOT USE!
   #:component-inline-methods ;; backward-compatibility only. DO NOT USE!
   #:component-operation-times ;; For internal use only.
   ;; portable ASDF encoding and implementation-specific external-format
   #:component-external-format #:component-encoding
   #:detect-encoding #:*encoding-detection-hook* #:always-default-encoding
   #:encoding-external-format #:*encoding-external-format-hook* #:default-encoding-external-format
   #:*default-encoding* #:*utf-8-external-format*))
(in-package :asdf/component)

(defgeneric* component-name (component)
  (:documentation "Name of the COMPONENT, unique relative to its parent"))
(defgeneric* component-system (component)
  (:documentation "Find the top-level system containing COMPONENT"))
(defgeneric* component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))
(defgeneric* component-relative-pathname (component)
  (:documentation "Returns a pathname for the component argument intended to be
interpreted relative to the pathname of that component's parent.
Despite the function's name, the return value may be an absolute
pathname, because an absolute pathname may be interpreted relative to
another pathname in a degenerate way."))
(defgeneric* component-property (component property))
#-gcl<2.7
(defgeneric* (setf component-property) (new-value component property))
(defgeneric* component-external-format (component))
(defgeneric* component-encoding (component))
(defgeneric* version-satisfies (component version))
(defgeneric* source-file-type (component system))

(with-upgrade (:when (find-class 'component nil))
  (defmethod reinitialize-instance :after ((c component) &rest initargs &key)
    (declare (ignorable c initargs)) (values)))

(defclass component ()
  ((name :accessor component-name :initarg :name :type string :documentation
         "Component name: designator for a string composed of portable pathname characters")
   ;; We might want to constrain version with
   ;; :type (and string (satisfies parse-version))
   ;; but we cannot until we fix all systems that don't use it correctly!
   (version :accessor component-version :initarg :version)
   (description :accessor component-description :initarg :description)
   (long-description :accessor component-long-description :initarg :long-description)
   (sibling-dependencies :accessor component-sibling-dependencies :initform nil)
   (if-feature :accessor component-if-feature :initform nil :initarg :if-feature)
   ;; In the ASDF object model, dependencies exist between *actions*
   ;; (an action is a pair of operation and component).
   ;; Dependencies are represented as alists of operations
   ;; to dependencies (other actions) in each component.
   ;; Up until ASDF 2.26.9, there used to be two kinds of dependencies:
   ;; in-order-to and do-first, each stored in its own slot. Now there is only in-order-to.
   ;; in-order-to used to represent things that modify the filesystem (such as compiling a fasl)
   ;; and do-first things that modify the current image (such as loading a fasl).
   ;; These are now unified because we now correctly propagate timestamps between dependencies.
   ;; Happily, no one seems to have used do-first too much, but the name in-order-to remains.
   ;; The names are bad, but they have been the official API since Dan Barlow's ASDF 1.52!
   ;; LispWorks's defsystem has caused-by and requires for in-order-to and do-first respectively.
   ;; Maybe rename the slots in ASDF? But that's not very backward-compatible.
   ;; See our ASDF 2 paper for more complete explanations.
   (in-order-to :initform nil :initarg :in-order-to
                :accessor component-in-order-to)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil) ;; OBSOLETE! DELETE THIS IF NO ONE USES.
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)
   ;; the absolute-pathname is computed based on relative-pathname...
   (absolute-pathname)
   (operation-times :initform (make-hash-table)
                    :accessor component-operation-times)
   (around-compile :initarg :around-compile)
   (%encoding :accessor %component-encoding :initform nil :initarg :encoding)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties :accessor component-properties :initarg :properties
               :initform nil)
   ;; For backward-compatibility, this slot is part of component rather than child-component
   (parent :initarg :parent :initform nil :reader component-parent)))

(defun* component-find-path (component)
  (check-type component (or null component))
  (reverse
   (loop :for c = component :then (component-parent c)
         :while c :collect (component-name c))))

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity nil)
    (format stream "~{~S~^ ~}" (component-find-path c))))

(defmethod component-system ((component component))
  (aif (component-parent component)
       (component-system it)
       component))


;;;; component pathnames

(defun* component-parent-pathname (component)
  ;; No default anymore (in particular, no *default-pathname-defaults*).
  ;; If you force component to have a NULL pathname, you better arrange
  ;; for any of its children to explicitly provide a proper absolute pathname
  ;; wherever a pathname is actually wanted.
  (let ((parent (component-parent component)))
    (when parent
      (component-pathname parent))))

(defmethod component-pathname ((component component))
  (if (slot-boundp component 'absolute-pathname)
      (slot-value component 'absolute-pathname)
      (let ((pathname
             (merge-pathnames*
              (component-relative-pathname component)
              (pathname-directory-pathname (component-parent-pathname component)))))
        (unless (or (null pathname) (absolute-pathname-p pathname))
          (error (compatfmt "~@<Invalid relative pathname ~S for component ~S~@:>")
                 pathname (component-find-path component)))
        (setf (slot-value component 'absolute-pathname) pathname)
        pathname)))

(defmethod component-relative-pathname ((component component))
  (coerce-pathname
   (or (slot-value component 'relative-pathname)
       (component-name component))
   :type (source-file-type component (component-system component)) ;; backward-compatibility
   :defaults (component-parent-pathname component)))


;;;; General component-property - ASDF3: remove? Define clean subclasses, not messy "properties".

(defmethod component-property ((c component) property)
  (cdr (assoc property (slot-value c 'properties) :test #'equal)))

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
    (if a
        (setf (cdr a) new-value)
        (setf (slot-value c 'properties)
              (acons property new-value (slot-value c 'properties)))))
  new-value)


;;;; Encodings

(defvar *default-encoding* :default
  "Default encoding for source files.
The default value :default preserves the legacy behavior.
A future default might be :utf-8 or :autodetect
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.")

(defparameter *utf-8-external-format*
  #+(and asdf-unicode (not clisp)) :utf-8
  #+(and asdf-unicode clisp) charset:utf-8
  #-asdf-unicode :default
  "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

(defun* always-default-encoding (pathname)
  (declare (ignore pathname))
  *default-encoding*)

(defvar *encoding-detection-hook* #'always-default-encoding
  "Hook for an extension to define a function to automatically detect a file's encoding")

(defun* detect-encoding (pathname)
  (if (and pathname (not (directory-pathname-p pathname)) (probe-file pathname))
      (funcall *encoding-detection-hook* pathname)
      *default-encoding*))

(defmethod component-encoding ((c component))
  (or (loop :for x = c :then (component-parent x)
        :while x :thereis (%component-encoding x))
      (detect-encoding (component-pathname c))))

(defun* default-encoding-external-format (encoding)
  (case encoding
    (:default :default) ;; for backward-compatibility only. Explicit usage discouraged.
    (:utf-8 *utf-8-external-format*)
    (otherwise
     (cerror "Continue using :external-format :default" (compatfmt "~@<Your ASDF component is using encoding ~S but it isn't recognized. Your system should :defsystem-depends-on (:asdf-encodings).~:>") encoding)
     :default)))

(defvar *encoding-external-format-hook*
  #'default-encoding-external-format
  "Hook for an extension to define a mapping between non-default encodings
and implementation-defined external-format's")

(defun* encoding-external-format (encoding)
  (funcall *encoding-external-format-hook* encoding))

(defmethod component-external-format ((c component))
  (encoding-external-format (component-encoding c)))


;;;; around-compile-hook

(defgeneric* around-compile-hook (component))
(defmethod around-compile-hook ((c component))
  (cond
    ((slot-boundp c 'around-compile)
     (slot-value c 'around-compile))
    ((component-parent c)
     (around-compile-hook (component-parent c)))))


;;;; version-satisfies

(defmethod version-satisfies ((c component) version)
  (unless (and version (slot-boundp c 'version))
    (when version
      (warn "Requested version ~S but component ~S has no version" version c))
    (return-from version-satisfies t))
  (version-satisfies (component-version c) version))

(defmethod version-satisfies ((cver string) version)
  (version-compatible-p cver version))
