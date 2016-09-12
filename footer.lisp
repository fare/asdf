;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(uiop/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/upgrade :asdf/find-system :asdf/operate :asdf/bundle)
  ;; Happily, all those implementations all have the same module-provider hook interface.
  #+(or abcl clasp cmucl clozure ecl mkcl sbcl)
  (:import-from #+abcl :sys #+(or clasp cmucl ecl) :ext #+clozure :ccl #+mkcl :mk-ext #+sbcl sb-ext
		#:*module-provider-functions*
		#+(or clasp ecl) #:*load-hooks*)
  #+mkcl (:import-from :si #:*load-hooks*))

(in-package :asdf/footer)

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.
#+(or abcl clasp clisp clozure cmucl ecl mkcl sbcl)
(with-upgradability ()
  #-clisp (pushnew 'module-provide-asdf *module-provider-functions*)
  #+clisp (if-let (x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom nil)))
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
  #+allegro
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* uiop/common-lisp::*acl-warn-save*))

  (dolist (f '(:asdf :asdf2 :asdf3 :asdf3.1 :asdf-package-system)) (pushnew f *features*))

  ;; Provide both lowercase and uppercase, to satisfy more people, especially LispWorks users.
  (provide "asdf") (provide "ASDF")

  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))
