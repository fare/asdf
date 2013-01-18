(defpackage :test-asdf-system
  (:use :cl :asdf :asdf/driver))
(in-package :test-asdf-system)

(defsystem :test-asdf/test9-1
    :version "1.1"
    :components ((:file "file2"))
    :depends-on ((:version :test-asdf/test9-2 "2.0")))

(defsystem :test-asdf/test9-2
  :version "1.0"
  :components ((:file "file1")))

(defsystem :test-asdf/test9-3
  :depends-on ((:version :test-asdf/test9-2 "1.0")))
