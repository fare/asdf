(in-package :asdf-test)
(defvar *od* 0)
(defsystem "overlapping-dependency"
  :components ((:file "overlapping-dependency"))
  ;;:perform (load-op (o c) (incf *od*))
  )
