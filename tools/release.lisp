(in-package :asdf-tools)

;;; Getting a list of source files in a system

(defun enough-namestring! (base pathname)
  (let ((e (enough-namestring base pathname)))
    (assert (relative-pathname-p e))
    e))

(defun enough-namestrings (base pathnames)
  (loop :with b = (ensure-pathname base :want-absolute t :want-directory t)
        :for p :in pathnames
        :collect (enough-namestring! p b)))

(defun system-source-files (system &key monolithic)
  (let* ((sys (find-system system))
         (components
           (required-components system
                                :other-systems monolithic
                                :goal-operation 'load-op
                                :keep-operation 'load-op
                                :keep-component 'file-component))
         (dir (ensure-pathname
               (system-source-directory sys)
               :want-absolute t :want-directory t))
         (pathnames (mapcar 'component-pathname components)))
    (enough-namestrings dir pathnames)))


;;; Making release tarballs for asdf, asdf/defsystem, uiop.

(defun tarname (name) (strcat name ".tar.gz"))

(defun make-tarball-under-build (name base files)
  (check-type name string)
  (ensure-pathname base :want-absolute t :want-existing t :want-directory t)
  (dolist (f files)
    (check-type f string))
  (let* ((base
           (ensure-pathname
            base
            :want-absolute t :want-directory t
            :want-existing t :truename t))
         (destination
           (ensure-pathname
            name
            :defaults (pn "build/")
            :want-relative t :ensure-absolute t
            :ensure-subpath t :ensure-directory t))
         (tarball
           (ensure-pathname
            (tarname name)
            :defaults (pn "build/")
            :want-relative t :ensure-absolute t
            :ensure-subpath t :want-file t
            :ensure-directories-exist t)))
    (assert (< 6 (length (pathname-directory destination))))
    (when (probe-file* destination)
      (error "Destination ~S already exists, not taking chances - you can delete it yourself."
             destination))
    (ensure-directories-exist destination)
    (run `(cp "-pHux" --parents ,@files ,destination) :directory base :show t)
    (run `(tar "zcfC" ,tarball ,(pn "build/") (,name /)) :show t)
    (delete-directory-tree destination :validate (lambda (x) (equal x destination)))
    (values)))

(defun driver-files ()
  (list* "README" "uiop.asd" "asdf-driver.asd" (system-source-files :uiop)))
(defun driver-name ()
  (format nil "uiop-~A" *version*))
(defun make-driver-tarball ()
  (make-tarball-under-build (driver-name) (pn "uiop/") (driver-files)))

(defun asdf-defsystem-files ()
  (list* "asdf.asd" "build/asdf.lisp" "version.lisp-expr" "header.lisp"
         (system-source-files :asdf/defsystem)))
(defun asdf-defsystem-name ()
  (format nil "asdf-defsystem-~A" *version*))
(defun make-asdf-defsystem-tarball ()
  (build-asdf)
  (make-tarball-under-build (asdf-defsystem-name) (pn) (asdf-defsystem-files)))

(defun asdf-git-name ()
  (strcat "asdf-" *version*))

(defun make-git-tarball ()
  (build-asdf)
  (with-asdf-dir ()
    (run `(tar zcf ("build/" ,(asdf-git-name) ".tar.gz") build/asdf.lisp ,@(run/lines '(git ls-files))
               (asdf-git-name)) :show t))
  (values))

(defun asdf-lisp-name ()
  (format nil "asdf-~A.lisp" *version*))

(defun make-asdf-lisp ()
  (build-asdf)
  (concatenate-files (list (pn "build/asdf.lisp"))
                     (pn "build/" (asdf-lisp-name))))

(defun make-archive ()
  (make-driver-tarball)
  (make-asdf-defsystem-tarball)
  (make-git-tarball)
  (make-asdf-lisp)
  (values))


;;; Publishing tarballs onto the public repository

(defvar *clnet* "common-lisp.net")
(defvar *clnet-asdf-public* "/project/asdf/public_html/")
(defun public-path (x) (strcat *clnet-asdf-public* x))

(defun publish-archive ()
  (let ((tarballs (mapcar 'tarname (list (driver-name) (asdf-defsystem-name) (asdf-git-name)))))
    (run `(rsync ,@tarballs ,(asdf-lisp-name) (,*clnet* ":" ,(public-path "archives/")))
         :show t :directory (pn "build/")))
  (format t "~&To download the tarballs, point your browser at:~%
        http://common-lisp.net/project/asdf/archives/
~%")
  (values))

(defun link-archive ()
  (run (format nil "ln -sf ~S ~S ; ln -sf ~S ~S ; ln -sf ~S ~S ; ln -sf ~S ~S"
               (tarname (driver-name))
               (public-path "archives/uiop.tar.gz")
               (tarname (asdf-defsystem-name))
               (public-path "archives/asdf-defsystem.tar.gz")
               (tarname (asdf-git-name))
               (public-path "archives/asdf.tar.gz")
               (asdf-lisp-name)
               (public-path "archives/asdf.lisp"))
       :show t :host *clnet*)
  (values))

(defun make-and-publish-archive ()
  (make-archive)
  (publish-archive)
  (link-archive))

(defun archive () "alias for make-and-publish-archive" (make-and-publish-archive))
(defun install () "alias for make-and-publish-archive" (make-and-publish-archive))


;;; Making a debian package
(defun debian-package (&optional (release "release"))
  (let* ((debian-version (debian-version-from-file release))
         (version (version-from-file release)))
    (unless (cl-ppcre:register-groups-bind (x epoch ver rel)
                ("^(([0-9]+):)?([0-9.]+)-([0-9]+)$" debian-version)
              (declare (ignorable x epoch rel))
              (equal ver version))
      (error "Debian version ~A doesn't match asdf version ~A" debian-version version))
    (clean)
    (format t "building package version ~A~%" (debian-version-from-file))
    (run `(git-buildpackage
           ;; --git-ignore-new ;; for testing purpose
           (--git-debian-branch= ,release)
           (--git-upstream-tag="%(version)s")
           ;;--git-upstream-tree=tag ;; if the changelog says 3.1.2, looks at that tag
           ;;(--git-upstream-branch= ,version) ;; if the changelog says 3.1.2, looks at that tag
           --git-tag --git-retag
           ;; --git-no-pristine-tar
           --git-force-create
           --git-ignore-branch)
         :directory (pn) :show t)))

(defun release ()
  "Release the code (not implemented)"
  #| RELEASE or PUSH checklist:
make test-all
make test-load-systems s=fare-all
make bump v=3.0
edit debian/changelog # RELEASE only...
git commit
git tag 3.0 # for example ...
make debian-package
git push
git push origin 3.0 # for example...
everything from here for RELEASE only
make release-push archive website debian-package
dput mentors ../*.changes
send debian mentors request
send announcement to asdf-announce, asdf-devel, etc.
Move all fixed bugs from Fix Committed -> Fix Released on launchpad
  |#
  (die 42 "release is not implemented yet"))

