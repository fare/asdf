;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(asdf/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :asdf/common-lisp :asdf/driver :asdf/upgrade
   :asdf/find-system :asdf/find-component :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/operate :asdf/bundle :asdf/concatenate-source
   :asdf/output-translations :asdf/source-registry
   :asdf/backward-internals :asdf/defsystem :asdf/backward-interface))
(in-package :asdf/footer)

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.

(with-upgradability ()
  #+(or abcl clisp clozure cmu ecl mkcl sbcl)
  (if-let (x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom nil)))
    (eval `(pushnew 'module-provide-asdf
                    #+abcl sys::*module-provider-functions*
                    #+clisp ,x
                    #+clozure ccl:*module-provider-functions*
                    #+(or cmu ecl) ext:*module-provider-functions*
                    #+mkcl mk-ext:*module-provider-functions*
                    #+sbcl sb-ext:*module-provider-functions*)))

  #+(or ecl mkcl)
  (progn
    (pushnew '("fasb" . si::load-binary) si:*load-hooks* :test 'equal :key 'car)

    #+(or (and ecl win32) (and mkcl windows))
    (unless (assoc "asd" #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* :test 'equal)
      (appendf #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* '(("asd" . si::load-source))))

    (setf #+ecl ext:*module-provider-functions* #+mkcl mk-ext::*module-provider-functions*
          (loop :for f :in #+ecl ext:*module-provider-functions*
                #+mkcl mk-ext::*module-provider-functions*
                :collect
                (if (eq f 'module-provide-asdf) f
                    #'(lambda (name)
                        (let ((l (multiple-value-list (funcall f name))))
                          (and (first l) (register-pre-built-system (coerce-name name)))
                          (values-list l))))))))


;;;; Done!
(with-upgradability ()
  #+allegro
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* asdf/common-lisp::*acl-warn-save*))

  (dolist (f '(:asdf :asdf2 :asdf3)) (pushnew f *features*))

  (provide :asdf)

  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))


