;; NB: this file is supposed to work using old defsystems
;; including not just ASDF2, but also legacy defsystems from allegro, genera, lispworks

(unless (find-package :asdf-test)
  (load (merge-pathnames
         (make-pathname :defaults *load-pathname*
                        :name "script-support" :directory '(:relative :back))
         *load-pathname*)))

(unless (find-package :asdf)
  (asdf-test::load-asdf)
  (asdf-test::frob-packages))

(in-package :asdf-test)

(DBG :foo)

(defparameter *eval-notes* ())
(defun note-eval (when file)
  (format t "~&XXX ~S ~S~%" when file)
  (push `(,when ,file #|,*load-pathname* ,*compile-file-pathname*|#) *eval-notes*))
(defun eval-notes ()
  (prog1 (reverse *eval-notes*) (setf *eval-notes* nil)))
(defmacro eval-note (&optional x)
  `(progn
     (eval-when (:compile-toplevel) (note-eval :compile-toplevel ',x))
     (eval-when (:load-toplevel) (note-eval :load-toplevel ',x))
     (eval-when (:execute) (note-eval :execute ',x))))


(eval-note :tsp)

(defvar *tsp* (asdf::pathname-directory-pathname *load-pathname*))

(defparameter asdf::*asdf-cache* nil) ;; disable any surrounding cache on ASDF3.

#+allegro
(excl:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp*)
  (:definitions
   "file1.lisp"
   "file2.lisp"))

#+genera
(sct:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp* :patchable nil)
  (:definitions
   "file1.lisp"
   "file2.lisp"))

#+lispworks
(scm:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp*)
  :members ("file1" "file2")
  :rules ((:in-order-to :compile ("file2")
           (:caused-by (:compile "file1"))
           (:requires (:load "file1")))))

#+asdf
(asdf:defsystem :test-stamp-propagation
  :pathname #.*tsp* :source-file nil
  :serial t
  :components
  ((:file "file1")
   (:file "file2")))

#+mk-defsystem
(mk:defsystem :test-stamp-propagation
  (:default-pathname #.*tsp* :patchable nil)
  (:serial
   "file1.lisp"
   "file2.lisp"))

(defvar *default-defsystem* (or #+(or allegro genera lispworks) :native
                                #+asdf :asdf
                                #+mk-defsystem :mk-defsystem))

(defun reload (&optional (defsystem *default-defsystem*))
  (format t "~&ASDF-CACHE ~S~%" asdf::*asdf-cache*)
  (setf *eval-notes* nil)
  (setf *compile-verbose* t *load-verbose* t)
  (ecase defsystem
    #+asdf
    (:asdf
     (note-eval :compiling :system)
     (asdf:compile-system :test-stamp-propagation)
     (note-eval :loading :system)
     (asdf:load-system :test-stamp-propagation))
    #+mk-defsystem
    (:mk-defsystem
     (note-eval :compiling :system)
     (mk:compile-system :test-stamp-propagation)
     (note-eval :loading :system)
     (mk:load-system :test-stamp-propagation))
    (:native
     (note-eval :compiling :system)
     #+allegro (excl:compile-system :test-stamp-propagation)
     #+lispworks (scm:compile-system :test-stamp-propagation)
     #+genera (sct:compile-system :test-stamp-propagation)
     (note-eval :loading :system)
     #+allegro (excl:load-system :test-stamp-propagation)
     #+lispworks (scm:load-system :test-stamp-propagation)
     #+genera (sct:load-system :test-stamp-propagation)))
  (let ((n (eval-notes)))
    (format t "~&EVAL-NOTES ~S~%" n)
    n))

(defun touch (filename)
  #+genera filename ;; do something with it!
  #-genera
  (uiop:run-program `("touch" ,(native-namestring filename))
                    :output t :error-output t))

(defun touch-file1.lisp ()
  (touch (asdf::subpathname *tsp* "file1.lisp")))

(defun touch-file1.fasl (&optional (defsystem *default-defsystem*))
  (touch (funcall
          (case defsystem (:asdf 'asdf::compile-file-pathname*) (t 'compile-file-pathname))
          (asdf::subpathname *tsp* "file1.lisp"))))

(defun sanitize-log (log)
  (remove-duplicates
   (remove '(:loading :system) log :test 'equal)
   :test 'equal :from-end t))

(defun test-defsystem (&optional (defsystem *default-defsystem*))
  (format t "~&Testing stamp propagation by defsystem ~S~%" defsystem)
  (DBG "loading system")
  (reload defsystem)
  (sleep 2)
  (DBG "touching first source file and reloading")
  (DBG "defsystem should recompile & reload everything")
  (touch-file1.lisp)
  (assert-equal (sanitize-log (reload defsystem))
                '((:compiling :system) (:compile-toplevel :file1) (:load-toplevel :file1)
                  (:compile-toplevel :file2) (:load-toplevel :file2)))
  (sleep 2)
  (DBG "touching first fasl file and reloading")
  (DBG "defsystem should reload it, recompile & reload the other")
  (touch-file1.fasl defsystem)
  (assert-equal (sanitize-log (reload defsystem))
                '((:compiling :system) (:load-toplevel :file1)
                  (:compile-toplevel :file2) (:load-toplevel :file2))))

(test-defsystem :asdf)

#+(or genera lispworks)
(test-defsystem :native)

#+(or allegro)
(signals error (test-defsystem :native))

#+mkdefsystem
(signals error (test-defsystem :mk-defsystem))
