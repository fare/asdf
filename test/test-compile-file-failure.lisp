(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; CLISP 2.48 has a bug that makes this test fail. Work around:
  #+(or clisp ecl) (when (and (eq asdf:*compile-file-failure-behaviour* :error)
                              #+ecl (equal (fasl-type) "fasc"))
                     (error 'asdf:compile-error :operation "op" :component "comp"))
  (warn "Warning."))
