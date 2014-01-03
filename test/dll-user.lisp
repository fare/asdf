(defpackage :test-asdf/dll-user (:use)) ;; dummy, for package-system dependencies.

(in-package :test-package)
(ffi:def-function "sample_function" () :returning :int)
