(in-package :asdf-tools)

;;; Using git
;; Note that the debian git at git://git.debian.org/git/pkg-common-lisp/cl-asdf.git is stale,
;; as we currently build directly from upstream at git://common-lisp.net/projects/asdf/asdf.git

(defun git (cmd &rest args)
  (with-asdf-dir ()
    (apply 'run (cons "git" cmd) args)))

(defun clean ()
  (git '(clean -xfd))
  (values))

(defun %push ()
  "Push git branches master and release to cl.net and master"
  (dolist (x '((status)
               (push --tags cl.net release master)
               (push --tags github release master)
               (fetch)
               (status)))
    (apply 'git x)))

(defun merge-master-into-release ()
  "Merge master into release"
  (dolist (x '((checkout master)
               (merge release)
               (checkout release)
               (merge master)
               (checkout master)))
    (apply 'git x)))

(defparameter *wrongful-tags*
  '("1.37" ;; It's not asdf.lisp 1.37, it's asdf.lisp 1.85! 1.37 was the CVS version of the README.
    "1.1720" ;; That was a typo for 1.720
    "RELEASE" "STABLE" ;; These were misguided attempts for what should have been branches
    "README" "emp")) ;; Mistakes

(defun fix-local-git-tags ()
  "Delete wrongful tags from local repository"
  (dolist (tag *wrongful-tags*)
    (git `(tag -d ,tag) :on-error t)))

(defun fix-remote-git-tags (&optional (remote "origin"))
  "Delete wrongful tags from remote repository"
  (dolist (tag *wrongful-tags*)
    (git `(push ,remote (:refs/tags/,tag)) :on-error t)))

(defun git-all-committed-p ()
  "Is your checkout clean, with all files committed?"
  (null (git '(status -s) :output :lines)))

(defun check-git-all-committed ()
  (or (git-all-committed-p)
      (die 2 "Your git checkout isn't clean and all committed:~%~A~%"
           (git '(status) :output :string))))
