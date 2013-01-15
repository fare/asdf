;;;; -----------------------------------------------------------------------
;;;; ASDF Footer: last words and cleanup

(asdf/package:define-package :asdf/footer
  (:recycle :asdf/footer :asdf)
  (:use :common-lisp :asdf/driver :asdf/upgrade
   :asdf/find-system :asdf/find-component :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/operate :asdf/bundle :asdf/concatenate-source
   :asdf/output-translations :asdf/source-registry
   :asdf/backward-internals :asdf/defsystem :asdf/backward-interface))
(in-package :asdf/footer)

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.

#+(or abcl clisp clozure cmu ecl mkcl sbcl)
(let ((x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom nil))))
  (when x
    (eval `(pushnew 'module-provide-asdf
            #+abcl sys::*module-provider-functions*
            #+clisp ,x
            #+clozure ccl:*module-provider-functions*
            #+(or cmu ecl) ext:*module-provider-functions*
            #+mkcl mk-ext:*module-provider-functions*
            #+sbcl sb-ext:*module-provider-functions*))))

#+(or ecl mkcl)
(progn
  (pushnew '("fasb" . si::load-binary) si:*load-hooks* :test 'equal :key 'car)

  #+(or (and ecl win32) (and mkcl windows))
  (unless (assoc "asd" #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* :test 'equal)
    (appendf #+ecl ext:*load-hooks* #+mkcl si::*load-hooks* '(("asd" . si::load-source))))

  (setf #+ecl ext:*module-provider-functions* #+mkcl mk-ext::*module-provider-functions*
        (loop :for f :in #+ecl ext:*module-provider-functions*
          #+mkcl mk-ext::*module-provider-functions*
          :unless (eq f 'module-provide-asdf)
          :collect #'(lambda (name)
                       (let ((l (multiple-value-list (funcall f name))))
                         (and (first l) (register-pre-built-system (coerce-name name)))
                         (values-list l))))))


;;;; Cleanups after hot-upgrade.
;; Things to do in case we're upgrading from a previous version of ASDF.
;; See https://bugs.launchpad.net/asdf/+bug/485687
;;
;; If a previous version of ASDF failed to read some configuration, try again.
(when *ignored-configuration-form*
  (clear-configuration)
  (setf *ignored-configuration-form* nil))

;;;; Done!

#+allegro
(eval-when (:compile-toplevel :execute)
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* asdf/compatibility::*acl-warn-save*)))

(dolist (f '(:asdf :asdf2 :asdf2.27)) (pushnew f *features*))

(provide :asdf)

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))

;;; Local Variables:
;;; mode: lisp
;;; End:
