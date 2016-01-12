(defsystem "asdf-tools"
  :description "tools to build, test, maintain and release ASDF itself"
  :depends-on ((:version "asdf" "3.1.6")
               (:version "inferior-shell" "2.0.3.4")
               (:version "lisp-invocation/all" "1.0.12")
               (:version "cl-ppcre" "2.0.11")
               (:version "optima.ppcre" "1.0")
               (:version "cl-scripting" "0.2")
               (:feature :sbcl (:require "sb-introspect")))
  :build-operation program-op
  :build-pathname #.(format nil "../build/asdf-tools~@[.~A~]"
                            (asdf/bundle:bundle-pathname-type :program))
  :entry-point "asdf-tools::entry-point"
  :components
  ((:file "package")
   (:file "main" :depends-on ("package"))
   (:file "pathnames" :depends-on ("package"))
   (:file "version" :depends-on ("package"))
   (:file "test-environment" :depends-on ("pathnames" "main"))
   (:file "build" :depends-on ("test-environment"))
   (:file "git" :depends-on ("test-environment"))
   (:file "test-basic" :depends-on ("test-environment"))
   (:file "test-scripts" :depends-on ("test-environment"))
   (:file "test-upgrade" :depends-on ("test-environment" "git"))
   (:file "test-all" :depends-on ("test-environment"))
   (:file "installation" :depends-on ("test-environment"))
   (:file "release" :depends-on ("version" "test-environment" "git"))))
