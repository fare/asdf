;;;; -------------------------------------------------------------------------
;;;; Systems

(uiop/package:define-package :asdf/system
  (:recycle :asdf :asdf/system)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/component)
  (:export
   #:system #:proto-system
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:reset-system
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:system-defsystem-depends-on #:system-depends-on #:system-weakly-depends-on
   #:component-build-pathname #:build-pathname
   #:component-entry-point #:entry-point
   #:homepage #:system-homepage
   #:bug-tracker #:system-bug-tracker
   #:mailto #:system-mailto
   #:long-name #:system-long-name
   #:source-control #:system-source-control
   #:find-system #:builtin-system-p)) ;; forward-reference, defined in find-system
(in-package :asdf/system)

(with-upgradability ()
  ;; The method is actually defined in asdf/find-system,
  ;; but we declare the function here to avoid a forward reference.
  (defgeneric find-system (system &optional error-p)
    (:documentation "Given a system designator, find the actual corresponding system object.
If no system is found, then signal an error if ERROR-P is true (the default), or else return NIL.
A system designator is usually a string (conventionally all lowercase) or a symbol, designating
the same system as its downcased name; it can also be a system object (designating itself)."))
  (defgeneric system-source-file (system)
    (:documentation "Return the source file in which system is defined."))
  ;; This is bad design, but was the easiest kluge I found to let the user specify that
  ;; some special actions create outputs at locations controled by the user that are not affected
  ;; by the usual output-translations.
  ;; TODO: Fix operate to stop passing flags to operation (which in the current design shouldn't
  ;; have any flags, since the stamp cache, etc., can't distinguish them), and instead insert
  ;; *there* the ability of specifying special output paths, not in the system definition.
  (defgeneric component-build-pathname (component)
    (:documentation "The COMPONENT-BUILD-PATHNAME, when defined and not null, specifies the
output pathname for the action using the COMPONENT-BUILD-OPERATION.

NB: This interface is subject to change. Please contact ASDF maintainers if you use it."))

  ;; TODO: Should this have been made a SYSTEM-ENTRY-POINT instead?
  (defgeneric component-entry-point (component)
    (:documentation "The COMPONENT-ENTRY-POINT, when defined, specifies what function to call
(with no argument) when running an image dumped from the COMPONENT.

NB: This interface is subject to change. Please contact ASDF maintainers if you use it."))
  (defmethod component-entry-point ((c component))
    nil))


;;;; The system class

(with-upgradability ()
  (defclass proto-system () ; slots to keep when resetting a system
    ;; To preserve identity for all objects, we'd need keep the components slots
    ;; but also to modify parse-component-form to reset the recycled objects.
    ((name) (source-file) #|(children) (children-by-names)|#)
    (:documentation "PROTO-SYSTEM defines the elements of identity that are preserved when
a SYSTEM is redefined and its class is modified."))

  (defclass system (module proto-system)
    ;; Backward-compatibility: inherit from module. ASDF4: only inherit from parent-component.
    (;; {,long-}description is now inherited from component, but we add the legacy accessors
     (description :accessor system-description)
     (long-description :accessor system-long-description)
     (author :accessor system-author :initarg :author :initform nil)
     (maintainer :accessor system-maintainer :initarg :maintainer :initform nil)
     (licence :accessor system-licence :initarg :licence
              :accessor system-license :initarg :license :initform nil)
     (homepage :accessor system-homepage :initarg :homepage :initform nil)
     (bug-tracker :accessor system-bug-tracker :initarg :bug-tracker :initform nil)
     (mailto :accessor system-mailto :initarg :mailto :initform nil)
     (long-name :accessor system-long-name :initarg :long-name :initform nil)
     ;; Conventions for this slot aren't clear yet as of ASDF 2.27, but whenever they are, they will be enforced.
     ;; I'm introducing the slot before the conventions are set for maximum compatibility.
     (source-control :accessor system-source-control :initarg :source-control :initform nil)
     (builtin-system-p :accessor builtin-system-p :initform nil :initarg :builtin-system-p)
     (build-pathname
      :initform nil :initarg :build-pathname :accessor component-build-pathname)
     (entry-point
      :initform nil :initarg :entry-point :accessor component-entry-point)
     (source-file :initform nil :initarg :source-file :accessor system-source-file)
     (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on
                           :initform nil)
     ;; these two are specially set in parse-component-form, so have no :INITARGs.
     (depends-on :reader system-depends-on :initform nil)
     (weakly-depends-on :reader system-weakly-depends-on :initform nil))
    (:documentation "SYSTEM is the base class for top-level components that users may request
ASDF to build."))


  (defun reset-system (system &rest keys &key &allow-other-keys)
    "Erase any data from a SYSTEM except its basic identity, then reinitialize it
based on supplied KEYS."
    (change-class (change-class system 'proto-system) 'system)
    (apply 'reinitialize-instance system keys)))


;;;; Pathnames

(with-upgradability ()
  ;; Resolve a system designator to a system before extracting its system-source-file
  (defmethod system-source-file ((system-name string))
    (system-source-file (find-system system-name)))
  (defmethod system-source-file ((system-name symbol))
    (when system-name
      (system-source-file (find-system system-name))))

  (defun system-source-directory (system-designator)
    "Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located."
    (pathname-directory-pathname (system-source-file system-designator)))

  (defun* (system-relative-pathname) (system name &key type)
    "Given a SYSTEM, and a (Unix-style relative path) NAME of a file (or directory) of given TYPE,
return the absolute pathname of a corresponding file under that system's source code pathname."
    (subpathname (system-source-directory system) name :type type))

  (defmethod component-pathname ((system system))
    "Given a SYSTEM, and a (Unix-style relative path) NAME of a file (or directory) of given TYPE,
return the absolute pathname of a corresponding file under that system's source code pathname."
    (let ((pathname (or (call-next-method) (system-source-directory system))))
      (unless (and (slot-boundp system 'relative-pathname) ;; backward-compatibility with ASDF1-age
                   (slot-value system 'relative-pathname)) ;; systems that directly access this slot.
        (setf (slot-value system 'relative-pathname) pathname))
      pathname))

  ;; The default method of component-relative-pathname for a system:
  ;; if a pathname was specified in the .asd file, it must be relative to the .asd file
  ;; (actually, to its truename* if *resolve-symlinks* it true, the default).
  ;; The method will return an *absolute* pathname, once again showing that the historical name
  ;; component-relative-pathname is misleading and should have been component-specified-pathname.
  (defmethod component-relative-pathname ((system system))
    (parse-unix-namestring
     (and (slot-boundp system 'relative-pathname)
          (slot-value system 'relative-pathname))
     :want-relative t
     :type :directory
     :ensure-absolute t
     :defaults (system-source-directory system)))

  ;; A system has no parent; if some method wants to make a path "relative to its parent",
  ;; it will instead be relative to the system itself.
  (defmethod component-parent-pathname ((system system))
    (system-source-directory system))

  ;; Most components don't have a specified component-build-pathname, and therefore
  ;; no magic redirection of their output that disregards the output-translations.
  (defmethod component-build-pathname ((c component))
    nil))

