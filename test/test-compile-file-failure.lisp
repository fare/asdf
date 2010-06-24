(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; CLISP 2.48 has a bug that makes this test fail. Work around:
  #+clisp (when (eq asdf:*compile-file-failure-behaviour* :error) (error 'asdf:compile-error))
  (warn "Warning."))
