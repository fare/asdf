;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(uiop/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/upgrade :asdf/find-system :asdf/operate :asdf/bundle))
(in-package :asdf/footer)

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.
#+(or abcl clasp clisp clozure cmucl ecl mkcl sbcl)
(with-upgradability ()
  (if-let (x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom nil)))
    (eval `(pushnew 'module-provide-asdf
                    #+abcl sys::*module-provider-functions*
                    #+(or clasp cmucl ecl) ext:*module-provider-functions*
                    #+clisp ,x
                    #+clozure ccl:*module-provider-functions*
                    #+mkcl mk-ext:*module-provider-functions*
                    #+sbcl sb-ext:*module-provider-functions*)))

  #+(or clasp ecl mkcl)
  (progn
    (pushnew '("fasb" . si::load-binary) si::*load-hooks* :test 'equal :key 'car)

    #+(or (and clasp windows) (and ecl win32) (and mkcl windows))
    (unless (assoc "asd" #+(or clasp ecl) ext:*load-hooks* #+mkcl si::*load-hooks* :test 'equal)
      (appendf #+(or clasp ecl) ext:*load-hooks* #+mkcl si::*load-hooks* '(("asd" . si::load-source))))

    (setf #+(or clasp ecl) ext:*module-provider-functions* #+mkcl mk-ext::*module-provider-functions*
          (loop :for f :in #+(or clasp ecl) ext:*module-provider-functions*
                #+mkcl mk-ext::*module-provider-functions*
                :collect
                (if (eq f 'module-provide-asdf) f
                    #'(lambda (name)
                        (let ((l (multiple-value-list (funcall f name))))
                          (and (first l) (register-preloaded-system (coerce-name name)))
                          (values-list l))))))))

#+cmucl ;; Hook into the CMUCL herald.
(with-upgradability ()
  (defun herald-asdf (stream)
    (format stream "    ASDF ~A" (asdf-version)))
  (setf (getf ext:*herald-items* :asdf) `(herald-asdf)))


;;;; Done!
(with-upgradability ()
  #+allegro
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* asdf/common-lisp::*acl-warn-save*))

  (dolist (f '(:asdf :asdf2 :asdf3 :asdf3.1 :asdf-package-system)) (pushnew f *features*))

  ;; Provide both lowercase and uppercase, to satisfy more people, especially LispWorks users.
  (provide "asdf") (provide "ASDF")

  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))
