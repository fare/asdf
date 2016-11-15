(defvar *tddoc* 0)
(incf *tddoc*)

(if (= *tddoc* 1)
    (defsystem "test-defsystem-depends-on-change"
      :defsystem-depends-on ("test-asdf/dep-can-change" "test-asdf/dep-can-disappear"))
    (defsystem "test-defsystem-depends-on-change"
      :defsystem-depends-on ("test-asdf/dep-can-change" "test-asdf/dep-can-appear")))
