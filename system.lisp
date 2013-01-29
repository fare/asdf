;;;; -------------------------------------------------------------------------
;;;; Systems

(asdf/package:define-package :asdf/system
  (:recycle :asdf :asdf/system)
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade :asdf/component)
  (:export
   #:system #:proto-system
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:reset-system
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:system-defsystem-depends-on
   #:component-build-pathname #:build-pathname
   #:component-entry-point #:entry-point
   #:find-system #:builtin-system-p)) ;; forward-reference, defined in find-system
(in-package :asdf/system)

(defgeneric* (find-system) (system &optional error-p))
(defgeneric* (system-source-file) (system)
  (:documentation "Return the source file in which system is defined."))
(defgeneric* component-build-pathname (component))

(defgeneric* component-entry-point (component))
(defmethod component-entry-point ((c component))
  (declare (ignorable c))
  nil)


;;;; The system class

(defclass proto-system () ; slots to keep when resetting a system
  ;; To preserve identity for all objects, we'd need keep the components slots
  ;; but also to modify parse-component-form to reset the recycled objects.
  ((name) (source-file) #|(children) (children-by-names)|#))

(defclass system (module proto-system)
  ;; Backward-compatibility: inherit from module. ASDF4: only inherit from parent-component.
  (;; {,long-}description is now inherited from component, but we add the legacy accessors
   (description :accessor system-description)
   (long-description :accessor system-long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence
            :accessor system-license :initarg :license)
   (builtin-system-p :accessor builtin-system-p :initform nil :initarg :builtin-system-p)
   (build-pathname
    :initform nil :initarg :build-pathname :accessor component-build-pathname)
   (entry-point
    :initform nil :initarg :entry-point :accessor component-entry-point)
   (source-file :initform nil :initarg :source-file :accessor system-source-file)
   (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on)))

(defun* reset-system (system &rest keys &key &allow-other-keys)
  (change-class (change-class system 'proto-system) 'system)
  (apply 'reinitialize-instance system keys))


;;;; Pathnames

(defmethod system-source-file ((system-name string))
  (system-source-file (find-system system-name)))
(defmethod system-source-file ((system-name symbol))
  (system-source-file (find-system system-name)))

(defun* system-source-directory (system-designator)
  "Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located."
  (pathname-directory-pathname (system-source-file system-designator)))

(defun* (system-relative-pathname) (system name &key type)
  (subpathname (system-source-directory system) name :type type))

(defmethod component-pathname ((system system))
  (let ((pathname (or (call-next-method) (system-source-directory system))))
    (unless (and (slot-boundp system 'relative-pathname) ;; backward-compatibility with ASDF1-age
                 (slot-value system 'relative-pathname)) ;; systems that directly access this slot.
      (setf (slot-value system 'relative-pathname) pathname))
    pathname))

(defmethod component-relative-pathname ((system system))
  (parse-unix-namestring
   (and (slot-boundp system 'relative-pathname)
        (slot-value system 'relative-pathname))
   :want-relative t
   :type :directory
   :ensure-absolute t
   :defaults (system-source-directory system)))

(defmethod component-parent-pathname ((system system))
  (system-source-directory system))

(defmethod component-build-pathname ((c component))
  (declare (ignorable c))
  nil)
