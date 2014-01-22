(defpackage :test-asdf/monodll-user (:use)) ;; dummy, for package-system dependencies.

(in-package :test-package)
(ffi:def-function "always_42" () :returning :int)
