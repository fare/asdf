(load "script-support")
(load "../asdf")

(defvar *test-directory*
  (ensure-directory-pathname
   (getenv "PWD")))

(defvar *test-conf-directory*
  (merge-pathnames "conf.d/" (getenv "PWD")))

(defun under-test-directory (path &optional (defaults *test-directory*))
  (merge-pathnames path defaults))

(defun ensure-recursive-directory (path)
  (concatenate
   'string
   (namestring
    (ensure-directory-pathname path))
   "/"))

(defun create-conf-files (&optional (path *test-conf-directory*))
  (let ((v `(("conf1.conf"
              ((:directory ,(namestring (under-test-directory "dir1/")))))
             ("conf2.conf"
              ((:tree ,(namestring (under-test-directory "dir2/"))))))))
    (loop
     :for (file contents) :in v :do
     (with-open-file (out (merge-pathnames file path)
                          :direction :output
                          :if-exists :supersede)
       (with-standard-io-syntax
         (format out "~{~S~}" contents))))))

(defvar *test-config-1*
  `(:source-registry
    (:tree ,(getenv "PWD"))
    (:ignore-inherited-configuration)))

(defvar *test-expect-1*
  (append
   (loop
    :for dir
    :in '("dir1/" "dir2/dir3/" "dir2/dir4/" "dir2/")
    :collect (merge-pathnames dir *test-directory*))
   (list *test-directory*)))

(defvar *test-source-registries*
  '(test-environment-source-registry
    test-environment-source-registry-recursive
    test-something-3))

(defun test-environment-source-registry ()
  (process-source-registry (getenv "CL_SOURCE_REGISTRY")
                           :inherit *test-source-registries*))

(defun test-environment-source-registry-recursive ()
  (process-source-registry
   (ensure-recursive-directory
    (getenv "CL_SOURCE_REGISTRY"))
   :inherit *test-source-registries*))

(defun test-directory-source-registry
    (&optional (directory *test-conf-directory*))
  (process-source-registry
   (validate-source-registry-directory directory)))

(defun test-something-3 () nil)

(cl-user::quit-on-error
 (create-conf-files)
 (assert (every #'pathname (test-environment-source-registry)))
 (assert (every #'pathname (test-environment-source-registry-recursive)))
 (assert (equal (test-directory-source-registry) *test-expect-1*))

 ;; FIXME:
 ;; (assert (equal (process-source-registry
 ;;                 *test-config-1*)
 ;;                *test-expect-1*))

 ;; FIXME: add more tests
 ;; (assert (equal ...))
 )