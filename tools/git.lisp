(in-package :asdf-tools)

;;; Using git
;; Note that the debian git at git://git.debian.org/git/pkg-common-lisp/cl-asdf.git is stale,
;; as we currently build directly from upstream at git://common-lisp.net/projects/asdf/asdf.git

(defun run* (cmd &rest keys)
  (multiple-value-bind (out err code)
      (apply 'run cmd keys)
    (if (eql 0 code)
        (success)
        (values nil out err code))))

(defun git (cmd &rest keys)
  (with-asdf-dir ()
    (apply 'run* (cons "git" cmd) keys)))

(defun clean ()
  "clean the checkout with git clean -xfd"
  (git '(clean -xfd)))

(defun %push ()
  "push git branches master and release upstream"
  (dolist (x '((status)
               (push --tags cl.net release master)
               (push --tags github release master)
               (fetch)
               (status)))
    (git x)))

(defun merge-master-into-release ()
  "merge git branch master into release"
  (dolist (x '((checkout master)
               (merge release)
               (checkout release)
               (merge master)
               (checkout master)))
    (git x)))

(defparameter *wrongful-tags*
  '("1.37" ;; It's not asdf.lisp 1.37, it's asdf.lisp 1.85! 1.37 was the CVS version of the README.
    "1.1720" ;; That was a typo for 1.720
    "RELEASE" "STABLE" ;; These were misguided attempts for what should have been branches
    "README" "emp")) ;; Mistakes

(defun fix-local-git-tags ()
  "delete wrongful tags from local git repository"
  (dolist (tag *wrongful-tags*)
    (git `(tag -d ,tag) :on-error nil)))

(defun fix-remote-git-tags (&optional (remote "origin"))
  "delete wrongful tags from remote git repository"
  (dolist (tag *wrongful-tags*)
    (git `(push ,remote (:refs/tags/,tag)) :on-error nil)))

(defun git-all-committed-p ()
  "is your checkout clean, with all files committed?"
  (null (nth-value 1 (git '(status -s) :output :lines))))

