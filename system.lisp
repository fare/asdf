;;;; -------------------------------------------------------------------------
;;;; Systems

(asdf/package:define-package :asdf/system
  (:recycle :asdf/system :asdf)
  (:use :common-lisp :asdf/utility :asdf/pathname :asdf/os :asdf/upgrade
   :asdf/component)
  (:export
   #:child-component #:parent-component #:module #:system
   #:component-children-by-name #:component-children #:compute-children-by-name
   #:module-default-component-class
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:reset-system #:builtin-system-p
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:find-system #:probe-asd ;; forward-reference, defined in find-system
   #:%set-system-source-file ;; For internal use only. DO NOT USE.
   #:module-components ;; backward-compatibility. DO NOT USE.
   #:system-defsystem-depends-on))
(in-package :asdf/system)

(defgeneric* find-system (system &optional error-p))
(declaim (ftype (function (t t) t) probe-asd))

;;;; Component hierarchy within a system
;; The tree typically but not necessarily follows the filesystem hierarchy.

(defclass child-component (component) ())

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

(with-upgrade (:when (find-class 'module nil))
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
      (compute-children-by-name m))
    (when (typep m 'system)
      (when (member 'source-file added)
        (%set-system-source-file
         (probe-asd (component-name m) (component-pathname m)) m))
      (when (equal (component-name m) "asdf")
        (setf (component-version m) (asdf-version))))))

(defclass module (child-component parent-component)
  ())


;;;; The system class itself

(defclass proto-system () ; slots to keep when resetting a system
  ;; To preserve identity for all objects, we'd need keep the components slots
  ;; but also to modify parse-component-form to reset the recycled objects.
  ((asdf/component::name) (source-file) #|(children) (children-by-names)|#))

(defclass system (module proto-system)
  ;; Backward-compatibility: inherit from module. ASDF3: only inherit from parent-component.
  (;; {,long-}description is now inherited from component, but we add the legacy accessors
   (asdf/component::description :accessor system-description)
   (asdf/component::long-description :accessor system-long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence
            :accessor system-license :initarg :license)
   (source-file :initarg :source-file :writer %set-system-source-file) ; upgrade issues on CLISP, CMUCL
   (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on)))

(defun* reset-system (system &rest keys &key &allow-other-keys)
  (change-class (change-class system 'proto-system) 'system)
  (apply 'reinitialize-instance system keys))


;;;; Pathnames

(defmethod component-pathname ((system system))
  (if (or (slot-boundp system 'asdf/component::relative-pathname)
            (slot-boundp system 'asdf/component::absolute-pathname)
            (slot-value system 'source-file))
    (call-next-method)
    (default-directory)))

(defgeneric* system-source-file (system)
  (:documentation "Return the source file in which system is defined."))
(defmethod system-source-file ((system system))
  ;; might be missing when upgrading from ASDF 1 and u-i-f-r-c failed
  (unless (slot-boundp system 'source-file)
    (%set-system-source-file
     (probe-asd (component-name system) (component-pathname system)) system))
  (slot-value system 'source-file))
(defmethod system-source-file ((system-name string))
  (system-source-file (find-system system-name)))
(defmethod system-source-file ((system-name symbol))
  (system-source-file (find-system system-name)))

(defun* system-source-directory (system-designator)
  "Return a pathname object corresponding to the
directory in which the system specification (.asd file) is
located."
  (pathname-directory-pathname (system-source-file system-designator)))

(defun* system-relative-pathname (system name &key type)
  (subpathname (system-source-directory system) name :type type))


;;;; Beware of builtin systems
(defgeneric* builtin-system-p (system))
(defmethod builtin-system-p ((s system))
  (let* ((system (find-system s nil))
         (sysdir (and system (component-pathname system)))
         (impdir (lisp-implementation-directory :truename t)))
    (and sysdir impdir (pathname-match-p (truename sysdir) (wilden impdir)) t)))

