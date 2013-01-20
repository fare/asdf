;;;; -------------------------------------------------------------------------
;;;; Components

(asdf/package:define-package :asdf/component
  (:recycle :asdf/component :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade)
  (:export
   #:component #:component-find-path
   #:component-name #:component-pathname #:component-relative-pathname
   #:component-parent #:component-system #:component-parent-pathname
   #:child-component #:parent-component #:module
   #:file-component
   #:source-file #:c-source-file #:java-source-file
   #:static-file #:doc-file #:html-file
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
   #:component-children-by-name #:component-children #:compute-children-by-name
   #:component-build-operation
   #:module-default-component-class
   #:module-components ;; backward-compatibility. DO NOT USE.

   ;; Internals we'd like to share with the ASDF package, especially for upgrade purposes
   #:name #:version #:description #:long-description #:author #:maintainer #:licence
   #:defsystem-depends-on
   #:sibling-dependencies #:if-feature #:in-order-to #:inline-methods
   #:relative-pathname #:absolute-pathname #:operation-times #:around-compile
   #:%encoding #:properties #:parent))
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

;;; Backward compatible way of computing the FILE-TYPE of a component.
;;; TODO: find users, have them stop using that.
(defgeneric* source-file-type (component system))

(when-upgrade (:when (find-class 'component nil))
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
   ;; ASDF3: get rid of these "component properties" ?
   (properties :accessor component-properties :initarg :properties
               :initform nil)
   ;; For backward-compatibility, this slot is part of component rather than child-component
   (parent :initarg :parent :initform nil :reader component-parent)
   (build-operation
    :initarg :build-operation :initform nil :reader component-build-operation)))

(defun* component-find-path (component)
  (check-type component (or null component))
  (reverse
   (loop :for c = component :then (component-parent c)
         :while c :collect (component-name c))))

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity nil)
    (format stream "~{~S~^ ~}" (component-find-path c))))

(defmethod component-system ((component component))
  (if-let (system (component-parent component))
    (component-system system)
    component))


;;;; Component hierarchy within a system
;; The tree typically but not necessarily follows the filesystem hierarchy.

(defclass child-component (component) ())

(defclass file-component (child-component)
  ((type :accessor file-type :initarg :type))) ; no default
(defclass source-file (file-component)
  ((type :initform nil))) ;; NB: many systems have come to rely on this default.
(defclass c-source-file (source-file)
  ((type :initform "c")))
(defclass java-source-file (source-file)
  ((type :initform "java")))
(defclass static-file (source-file)
  ((type :initform nil)))
(defclass doc-file (static-file) ())
(defclass html-file (doc-file)
  ((type :initform "html")))

(defclass parent-component (component)
  ((children
    :initform nil
    :initarg :components
    :reader module-components ; backward-compatibility
    :accessor component-children)
   (children-by-name
    :reader module-components-by-name ; backward-compatibility
    :accessor component-children-by-name)
   (default-component-class
    :initform nil
    :initarg :default-component-class
    :accessor module-default-component-class)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun* compute-children-by-name (parent &key only-if-needed-p)
    (unless (and only-if-needed-p (slot-boundp parent 'children-by-name))
      (let ((hash (make-hash-table :test 'equal)))
        (setf (component-children-by-name parent) hash)
        (loop :for c :in (component-children parent)
              :for name = (component-name c)
              :for previous = (gethash name hash)
              :do (when previous (error 'duplicate-names :name name))
                  (setf (gethash name hash) c))
        hash))))

(when-upgrade (:when (find-class 'module nil))
  (defmethod reinitialize-instance :after ((m module) &rest initargs &key)
    (declare (ignorable m initargs)) (values))
  (defmethod update-instance-for-redefined-class :after
      ((m module) added deleted plist &key)
    (declare (ignorable m added deleted plist))
    (when (and (member 'children added) (member 'components deleted))
      (setf (slot-value m 'children)
            ;; old ECLs provide an alist instead of a plist(!)
            (if (or #+ecl (consp (first plist))) (or #+ecl (cdr (assoc 'components plist)))
                (getf plist 'components)))
      (compute-children-by-name m))))

(defclass module (child-component parent-component)
  ())


;;;; component pathnames

(defgeneric* (component-parent-pathname) (component))
(defmethod component-parent-pathname (component)
  (component-pathname (component-parent component)))

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
  ;; source-file-type is backward-compatibility with ASDF1;
  ;; we ought to be able to extract this from the component alone with COMPONENT-TYPE.
  ;; TODO: track who uses it, and have them not use it anymore.
  (parse-unix-namestring
   (or (and (slot-boundp component 'relative-pathname)
            (slot-value component 'relative-pathname))
       (component-name component))
   :want-relative t
   :type (source-file-type component (component-system component))
   :defaults (component-parent-pathname component)))

(defmethod source-file-type ((component parent-component) system)
  (declare (ignorable component system))
  :directory)

(defmethod source-file-type ((component file-component) system)
  (declare (ignorable system))
  (file-type component))


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

(defmethod component-encoding ((c component))
  (or (loop :for x = c :then (component-parent x)
        :while x :thereis (%component-encoding x))
      (detect-encoding (component-pathname c))))

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
