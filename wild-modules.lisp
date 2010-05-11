(in-package :asdf)

(defclass wild-module (module)
  ((component-class :accessor wild-module-component-class
                    :initform 'static-file :initarg :component-class)
   (component-options :accessor wild-module-component-options
                      :initform nil :initarg :component-options)))

(defmethod (setf module-components) (new-value (module wild-module))
  (when  new-value
    (sysdef-error "Cannot explicitly set wild-module ~A's components. Please ~
use a wild pathname instead." module)))

(defmethod reinitialize-instance :after ((self wild-module) &key)
  (let ((pathname (slot-value self 'relative-pathname)))
    (and pathname
         (not (wild-pathname-p pathname))
         (sysdef-error "Wild-module ~A specified with non-wild pathname ~A."
                       self pathname))
    (setf (slot-value self 'components)
          (let* ((*default-pathname-defaults* (component-parent-pathname self))
                 (files (directory (merge-pathnames (component-relative-pathname self))))
                 (class (wild-module-component-class self))
                 (options (wild-module-component-options self)))
            (mapcar (lambda (file)
                      (apply #'make-instance class
                             :name (file-namestring file)
                                        ;; XXX fails when wildcards are in
                                        ;; the directory or higher parts.
                             :pathname file
                             :parent self
                             options))
                    files)))))

;; Don't export wild-module or else will get a full warning
;; when (require :asdf) if asdf is already loaded

;;(export '(wild-module))
