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

(defun make-tarball-from-git (name base files &key (type "tar.gz"))
  (with-asdf-dir (base)
    (let ((tarball (strcat name "." type)))
      (run `(git archive -o ,(pn "build" tarball)
                 :prefix (,name /) ,*version* -- ,@files) :show t)))
  (success))

(defun make-tarball-from-git-plus (name base files &key asdf-lisp version-file)
  ;; make a tarball, then add build/asdf.lisp to it
  (make-tarball-from-git name base files :type "tar")
  (let* ((tarball (strcat name ".tar")))
    (with-asdf-dir ("build/")
      (when asdf-lisp
        (build-asdf)
        (ensure-directories-exist (pn "build" name "build/"))
        (run `(cp -a asdf.lisp (,name /build/)))
        ;; TODO: find which tar it is and tell --uid 0 --gid 0 to BSD tar
        ;; and --owner root --group root to GNU tar,
        ;; falling back to nothing. Sigh.
        (run `(env "COPYFILE_DISABLE=1"
                   tar "-rf" ,tarball (,name /build/asdf.lisp)) :show t)
        (run `(rm -f (,name /build/asdf.lisp)))
        (delete-empty-directory (pn "build" name "build/")))
      (when version-file
        (ensure-directories-exist (pn "build" name ""))
        (run `(cp -a ../version.lisp-expr (,name /)))
        (run `(env "COPYFILE_DISABLE=1"
                   tar "-rf" ,tarball (,name /version.lisp-expr)) :show t)
        (run `(rm -f (,name /version.lisp-expr))))
      (let ((dir (pn "build" name "")))
        (when (directory-exists-p dir) (delete-empty-directory dir)))
      (run `(gzip -f9 ,tarball) :show t)))
  (success))

(defun uiop-files ()
  "list files in uiop"
  (list* "README" "uiop.asd" "asdf-driver.asd" (system-source-files :uiop)))
(defun driver-name ()
  (format nil "uiop-~A" *version*))
(defun make-driver-tarball ()
  (make-tarball-from-git-plus
   (driver-name) "uiop/"
   (remove "version.lisp-expr" (uiop-files) :test 'equal)
   :version-file t))


(defun asdf-defsystem-files ()
  "list files in asdf/defsystem"
  (list* "asdf.asd" "version.lisp-expr" "header.lisp"
         (system-source-files :asdf/defsystem)))
(defun asdf-defsystem-name ()
  (format nil "asdf-defsystem-~A" *version*))
(defun make-asdf-defsystem-tarball ()
  (make-tarball-from-git-plus
   (asdf-defsystem-name) "" (asdf-defsystem-files)))

(defun asdf-git-name ()
  (strcat "asdf-" *version*))

(defun make-git-tarball ()
  (make-tarball-from-git-plus (asdf-git-name) "" nil))

(defun asdf-lisp-name ()
  (format nil "asdf-~A.lisp" *version*))

(defun make-asdf-lisp ()
  (build-asdf)
  (concatenate-files (list (pn "build/asdf.lisp"))
                     (pn "build/" (asdf-lisp-name)))
  (success))

(defun make-archive ()
  "build tarballs for release"
  (make-driver-tarball)
  (make-asdf-defsystem-tarball)
  (make-git-tarball)
  (make-asdf-lisp)
  (success))


;;; Publishing tarballs onto the public repository

(defvar *clnet* "common-lisp.net")
(defvar *clnet-asdf-public* "/project/asdf/public_html/")
(defun public-path (x) (strcat *clnet-asdf-public* x))

(defun publish-archive ()
  "publish tarballs to the website"
  (let ((tarballs (mapcar 'tarname (list (driver-name) (asdf-defsystem-name) (asdf-git-name)))))
    (run `(rsync "--times" "--chmod=a+rX,ug+w"
                 ,@tarballs ,(asdf-lisp-name) (,*clnet* ":" ,(public-path "archives/")))
         :show t :directory (pn "build/")))
  (format t "~&To download the tarballs, point your browser at:~%
        http://common-lisp.net/project/asdf/archives/
~%")
  (success))

(defun link-archive ()
  "symlink new tarballs on the website"
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
  (success))

(defun make-and-publish-archive ()
  "make and publish tarballs"
  (make-archive)
  (publish-archive)
  (link-archive))

(defalias archive make-and-publish-archive)
(defalias install make-and-publish-archive)


;;; Making a debian package
(defun debian-package (&optional (release "release"))
  "build a debian package"
  (let* ((debian-version (debian-version-from-file release))
         (version (version-from-file release)))
    (unless (equal version (parse-debian-version debian-version))
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
         :directory (pn) :show t))
  (success))

(defun debian-architecture ()
  (run/ss `(dpkg --print-architecture)))

(defun publish-debian-package (&optional release)
  "publish a debian package"
  (let ((changes (strcat "cl-asdf_" (debian-version-without-epoch (debian-version-from-file release))
                         "_" (debian-architecture) ".changes")))
    (run* `(dput mentors ,(pn "../" changes)))))

(deftestcmd release (new-version lisps scripts systems)
  "all steps to release the code (NOT YET IMPLEMENTED)"
  (break) ;; for each function, offer to do it or not (?)
  (with-asdf-dir ()
    (let ((log (newlogfile "release" "all"))
          (releasep (= (length (parse-version new-version)) 3)))
      (when releasep
        (let ((debian-version (debian-version-from-file)))
          (unless (equal new-version (parse-debian-version debian-version))
            (error "You're trying to release version ~A but the debian/changelog wasn't properly updated"
                   new-version)))
        (when (nth-value 1 (run '(parse-changelog debian/changelog) :output nil :error-output :lines))
          (error "Malformed debian/changelog entry")))
      scripts ;; TODO: needs to be passed as argument!
      (and ;; need a better combinator, that tells us about progress, etc.
       (git-all-committed-p)
       (test-all-no-stop) ;; TODO: NEED ARGUMENTS!
       (test-load-systems lisps systems)
       (bump new-version)
       (when releasep
         (and
          (debian-package)
          (publish-debian-package)
          (merge-master-into-release)))
       ;; SUCCESS! now publish more widely
       (%push)
       (archive)
       (website)
       (when releasep
         (log! log t "Don't forget to send a debian mentors request!"))
       (log! log "Don't forget to send announcement to asdf-announce, asdf-devel, etc.")
       (log! log "Don't forget to move all fixed bugs from Fix Committed -> Fix Released on launchpad"))))
  (success))
