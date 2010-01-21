(load "script-support")
(load "../asdf")

;; TODO:
;; - test for conditions
;; - use with-env for CL_SOURCE_REGISTRY
;; - write clean-up procedures, if needed

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require :sb-posix))

(defpackage :test-source-registry
  (:use #:cl #:asdf))

(in-package :test-source-registry)

(defun getenv (x)
  #+sbcl
  (sb-posix:getenv x))

(defun setenv (x v)
  #+sbcl
  (sb-posix:putenv
   (concatenate 'string x "=" v))
  v)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-env ((&rest kv) &body body)
  (let ((temps (loop :for i :upto (length kv) :collect (gensym)))
        (keys (mapcar #'(lambda (x) (first x)) kv)))
    (flet ((gen-getenvs ()
             (mapcar #'(lambda (x y)
                         `(,y (getenv ,x)))
                     keys temps))
           (gen-setenvs ()
             (mapcar #'(lambda (x y) `(setenv ,y ,x))
                     temps keys)))
      `(let ,(gen-getenvs)
         (unwind-protect
              (progn
                ,@(loop :for (k v) :in kv :collect `(setenv ,k ,v))
                ,@body)
           (progn ,@(gen-setenvs)))))))

(defun make-temporary-directory ()
  (let ((str (with-output-to-string (s)
               (sb-ext:run-program "mktemp" '("-d" "-u") :search t :output s))))
    (concatenate 'string
                 (string-trim '(#\newline) str) "/")))

(defvar *base-directory* (make-temporary-directory))

(defun home-directory (&optional (base *base-directory*))
  (let ((s (namestring (user-homedir-pathname))))
    (merge-pathnames (subseq s 1 (length s)) base)))
(defun system-directory (&optional (base *base-directory*))
  (merge-pathnames "etc/" base))
(defun tmp-directory (&optional (base *base-directory*))
  (merge-pathnames "tmp/" base))

(defun home-source-registry ()
  (asdf::source-registry-under (merge-pathnames ".config/" (home-directory))))
(defun system-source-registry ()
  (asdf::source-registry-under (system-directory)))

(defun merge-temp-under (path)
  (namestring
   (merge-pathnames
    (let ((s (string-downcase
              (prin1-to-string (gentemp)))))
      (concatenate 'string s "/"))
    path)))
(defvar *asd-temp-path-1*
  (merge-temp-under
   (merge-temp-under (tmp-directory))))
(defvar *system-source-registry-config*
  `(:source-registry
    (:inherit-configuration)
    (:tree ,*asd-temp-path-1*)))
(defvar *asd-temp-path-2*
  (namestring (tmp-directory)))
(defvar *home-source-registry-config*
  `(:source-registry
    (:inhert-configuration)
    (:directory ,(namestring (tmp-directory)))))

(defun write-registry-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax 
      (format out "~S" contents))))
(defun write-system-source-registry-config
    (&key (path (system-source-registry)) (contents *system-source-registry-config*))
  (write-registry-file path contents))
(defun write-home-source-registry-config
    (&key (path (home-source-registry)) (contents *home-source-registry-config*))
  (write-registry-file path contents))

;; use *asd-temp-path-X*?
(defun create-asd-file (&optional (directory *default-pathname-defaults*))
  (let ((*default-pathname-defaults* directory))
    (with-open-file (out 
  

;; test environment variable
(defun test-environment ()
  (with-env-var (("CL_SOURCE_REGISTRY"
                  (format nil "~A:~A" (home-directory) (system-directory))))
    ...))

;; test files
(defun test-files ()
  ...)

;; (cl-user::quit-on-error
;; ...)