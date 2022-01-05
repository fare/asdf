;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(uiop/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/system ;; used by ECL
        :asdf/upgrade :asdf/system-registry :asdf/operate :asdf/bundle)
  ;; Happily, all those implementations all have the same module-provider hook interface.
  #+(or abcl clasp cmucl clozure ecl mezzano mkcl sbcl)
  (:import-from #+abcl :sys #+(or clasp cmucl ecl) :ext #+clozure :ccl #+mkcl :mk-ext #+sbcl sb-ext #+mezzano :sys.int
                #:*module-provider-functions*
                #+ecl #:*load-hooks*)
  #+(or clasp mkcl) (:import-from :si #:*load-hooks*))

(in-package :asdf/footer)

;;;; Register ASDF itself and all its subsystems as preloaded.
(with-upgradability ()
  (dolist (s '("asdf" "asdf-package-system"))
    ;; Don't bother with these system names, no one relies on them anymore:
    ;; "asdf-utils" "asdf-bundle" "asdf-driver" "asdf-defsystem"
    (register-preloaded-system s :version *asdf-version*))
  (register-preloaded-system "uiop" :version *uiop-version*))

;;;; Ensure that the version slot on the registered preloaded systems are
;;;; correct, by CLEARing the system. However, we do not CLEAR-SYSTEM
;;;; unconditionally. This is because it's possible the user has upgraded the
;;;; systems using ASDF itself, meaning that the registered systems have real
;;;; data from the file system that we want to preserve instead of blasting
;;;; away and replacing with a blank preloaded system.
(with-upgradability ()
  (unless (equal (system-version (registered-system "asdf")) (asdf-version))
    (clear-system "asdf"))
  ;; 3.1.2 is the last version where asdf-package-system was a separate system.
  (when (version< "3.1.2" (system-version (registered-system "asdf-package-system")))
    (clear-system "asdf-package-system"))
  (unless (equal (system-version (registered-system "uiop")) *uiop-version*)
    (clear-system "uiop")))

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.
#+(or abcl clasp clisp clozure cmucl ecl mezzano mkcl sbcl)
(with-upgradability ()
  ;; Hook into CL:REQUIRE.
  #-clisp (pushnew 'module-provide-asdf *module-provider-functions*)
  #+clisp (if-let (x (find-symbol* '#:*module-provider-functions* :custom nil))
            (eval `(pushnew 'module-provide-asdf ,x)))

  #+(or clasp ecl mkcl)
  (progn
    (pushnew '("fasb" . si::load-binary) *load-hooks* :test 'equal :key 'car)

    #+os-windows
    (unless (assoc "asd" *load-hooks* :test 'equal)
      (appendf *load-hooks* '(("asd" . si::load-source))))

    ;; Wrap module provider functions in an idempotent, upgrade friendly way
    (defvar *wrapped-module-provider* (make-hash-table))
    (setf (gethash 'module-provide-asdf *wrapped-module-provider*) 'module-provide-asdf)
    (defun wrap-module-provider (provider name)
      (let ((results (multiple-value-list (funcall provider name))))
        (when (first results) (register-preloaded-system (coerce-name name)))
        (values-list results)))
    (defun wrap-module-provider-function (provider)
      (ensure-gethash provider *wrapped-module-provider*
                      (constantly
                       #'(lambda (module-name)
                           (wrap-module-provider provider module-name)))))
    (setf *module-provider-functions*
          (mapcar #'wrap-module-provider-function *module-provider-functions*))))

#+cmucl ;; Hook into the CMUCL herald.
(with-upgradability ()
  (defun herald-asdf (stream)
    (format stream "    ASDF ~A" (asdf-version)))
  (setf (getf ext:*herald-items* :asdf) '(herald-asdf)))


;;;; Done!
(with-upgradability ()
  #+allegro ;; restore *w-o-n-r-c* setting as saved in uiop/common-lisp
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* uiop/common-lisp::*acl-warn-save*))

  ;; Advertise the features we provide.
  (dolist (f '(:asdf :asdf2 :asdf3 :asdf3.1 :asdf3.2 :asdf3.3)) (pushnew f *features*))

  ;; Provide both lowercase and uppercase, to satisfy more people, especially LispWorks users.
  (provide "asdf") (provide "ASDF")

  ;; Finally, call a function that will cleanup in case this is an upgrade of an older ASDF.
  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))
