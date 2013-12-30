(in-package :asdf)

(defsystem package-system-test
  :class package-system
  :defsystem-depends-on
  #.(unless (find-class 'package-system nil) '(:asdf-package-system)))
