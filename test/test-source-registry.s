(load "script-support")
(load "../asdf")

;; TODO:
;; - test for directories
;; - test for correct chaining of inheritance

(defun pathname->directory (pathname)
  (make-pathname
   :directory (append (pathname-directory pathname)
                      (list (file-namestring pathname)))
   :name nil
   :type nil
   :defaults (pathname pathname)))

(defvar *test-directory*
  (pathname->directory (getenv "TEST_DIR")))

(defun under-test-directory (path &optional (defaults *test-directory*))
  (merge-pathnames path defaults))

(defvar *test-config-1*
  `(:source-registry
    (:directory ,(namestring (under-test-directory "dir1/")))
    (:tree ,(namestring (under-test-directory "dir2/")))
    (:ignore-inherited-configuration)))

(defvar *test-expect-1*
  (loop
   :for dir
   :in '("dir1/" "dir2/dir3/" "dir2/dir4/" "dir2/")
   :collect (merge-pathnames dir *test-directory*)))

(defvar *test-source-registries*
  '(test-environment-source-registry
    test-something-2
    test-something-3))

(cl-user::quit-on-error
 (assert (equal (process-source-registry
                 (getenv "CL_SOURCE_REGISTRY"))
                *test-expect-1*))
 (assert (equal (process-source-registry
                 *test-config-1*)
                *test-expect-1*))
 ;; FIXME: add more tests
 ;; (assert (equal ...))
 )