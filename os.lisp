;;;; ---------------------------------------------------------------------------
;;;; Access to the Operating System

(asdf/package:define-package :asdf/os
  (:recycle :asdf/os :asdf)
  (:use :cl :asdf/package :asdf/compatibility :asdf/utility :asdf/pathname :asdf/stream)
  (:export
   #:featurep #:os-unix-p #:os-windows-p ;; features
   #:getenv #:getenvp ;; environment variables
   #:inter-directory-separator #:split-pathnames*
   #:getenv-pathname #:getenv-pathnames
   #:getenv-absolute-directory #:getenv-absolute-directories
   #:implementation-identifier ;; implementation identifier
   #:implementation-type #:operating-system #:architecture #:lisp-version-string
   #:hostname #:user-homedir #:lisp-implementation-directory
   #:getcwd #:chdir #:call-with-current-directory #:with-current-directory
   #:*temporary-directory* #:temporary-directory #:default-temporary-directory
   #:setup-temporary-directory
   #:call-with-temporary-file #:with-temporary-file))
(in-package :asdf/os)

;;; Features
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun* featurep (x &optional (*features* *features*))
    (cond
      ((atom x) (and (member x *features*) t))
      ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
      ((eq :or (car x)) (some #'featurep (cdr x)))
      ((eq :and (car x)) (every #'featurep (cdr x)))
      (t (error "Malformed feature specification ~S" x))))

  (defun* os-unix-p ()
    (featurep '(:or :unix :cygwin :darwin)))

  (defun* os-windows-p ()
    (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32))))

  (defun* detect-os ()
    (flet ((yes (yes) (pushnew yes *features*))
           (no (no) (setf *features* (remove no *features*))))
      (cond
        ((os-unix-p) (yes :os-unix) (no :os-windows))
        ((os-windows-p) (yes :os-windows) (no :os-unix))
        (t (error "Congratulations for trying XCVB on an operating system~%~
that is neither Unix, nor Windows.~%Now you port it.")))))

  (detect-os))

;;;; Environment variables: getting them, and parsing them.

(defun* getenv (x)
  (declare (ignorable x))
  #+(or abcl clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+gcl (system:getenv x)
  #+genera nil
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+mkcl (#.(or (find-symbol* 'getenv :si nil) (find-symbol* 'getenv :mk-ext nil)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S is not supported on your implementation" 'getenv))

(defun* getenvp (x)
  "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
  (let ((g (getenv x))) (and (not (emptyp g)) g)))

(defun* inter-directory-separator ()
  (if (os-unix-p) #\: #\;))
(defun* split-pathnames* (x want-absolute want-directory fmt &rest args)
  (loop :for dir :in (split-string
                      x :separator (string (inter-directory-separator)))
        :collect (apply 'ensure-pathname* dir want-absolute want-directory fmt args)))
(defun* getenv-pathname (x &key want-absolute want-directory &aux (s (getenv x)))
  (ensure-pathname* s want-absolute want-directory "from (getenv ~S)" x))
(defun* getenv-pathnames (x &key want-absolute want-directory &aux (s (getenv x)))
  (and (plusp (length s))
       (split-pathnames* s want-absolute want-directory "from (getenv ~S) = ~S" x s)))
(defun* getenv-absolute-directory (x)
  (getenv-pathname x :want-absolute t :want-directory t))
(defun* getenv-absolute-directories (x)
  (getenv-pathnames x :want-absolute t :want-directory t))


;;;; implementation-identifier
;;
;; produce a string to identify current implementation.
;; Initially stolen from SLIME's SWANK, completely rewritten since.
;; We're back to runtime checking, for the sake of e.g. ABCL.

(defun* first-feature (feature-sets)
  (dolist (x feature-sets)
    (multiple-value-bind (short long feature-expr)
        (if (consp x)
            (values (first x) (second x) (cons :or (rest x)))
            (values x x x))
      (when (featurep feature-expr)
        (return (values short long))))))

(defun* implementation-type ()
  (first-feature
   '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp)
     (:cmu :cmucl :cmu) :ecl :gcl
     (:lwpe :lispworks-personal-edition) (:lw :lispworks)
     :mcl :mkcl :sbcl :scl (:smbx :symbolics) :xcl)))

(defun* operating-system ()
  (first-feature
   '(:cygwin (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
     (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
     (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
     (:solaris :solaris :sunos) (:bsd :bsd :freebsd :netbsd :openbsd) :unix
     :genera)))

(defun* architecture ()
  (first-feature
   '((:x64 :x86-64 :x86_64 :x8664-target :amd64 (:and :word-size=64 :pc386))
     (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
     (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
     :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
     :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
     ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
     ;; we may have to segregate the code still by architecture.
     (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

#+clozure
(defun* ccl-fasl-version ()
  ;; the fasl version is target-dependent from CCL 1.8 on.
  (or (let ((s 'ccl::target-fasl-version))
        (and (fboundp s) (funcall s)))
      (and (boundp 'ccl::fasl-version)
           (symbol-value 'ccl::fasl-version))
      (error "Can't determine fasl version.")))

(defun* lisp-version-string ()
  (let ((s (lisp-implementation-version)))
    (car ; as opposed to OR, this idiom prevents some unreachable code warning
     (list
      #+allegro
      (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
              excl::*common-lisp-version-number*
              ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
              (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
              ;; Note if not using International ACL
              ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
              (excl:ics-target-case (:-ics "8"))
              (and (member :smp *features*) "S"))
      #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
      #+clisp
      (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
      #+clozure
      (format nil "~d.~d-f~d" ; shorten for windows
              ccl::*openmcl-major-version*
              ccl::*openmcl-minor-version*
              (logand (ccl-fasl-version) #xFF))
      #+cmu (substitute #\- #\/ s)
      #+scl (format nil "~A~A" s
                    ;; ANSI upper case vs lower case.
                    (ecase ext:*case-mode* (:upper "") (:lower "l")))
      #+ecl (format nil "~A~@[-~A~]" s
                    (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                      (subseq vcs-id 0 (min (length vcs-id) 8))))
      #+gcl (subseq s (1+ (position #\space s)))
      #+genera
      (multiple-value-bind (major minor) (sct:get-system-version "System")
        (format nil "~D.~D" major minor))
      #+mcl (subseq s 8) ; strip the leading "Version "
      s))))

(defun* implementation-identifier ()
  (substitute-if
   #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
   (format nil "~(~a~@{~@[-~a~]~}~)"
           (or (implementation-type) (lisp-implementation-type))
           (or (lisp-version-string) (lisp-implementation-version))
           (or (operating-system) (software-type))
           (or (architecture) (machine-type)))))


;;;; Other system information

(defun* hostname ()
  ;; Note: untested on RMCL
  #+(or abcl clozure cmucl ecl genera lispworks mcl mkcl sbcl scl xcl) (machine-instance)
  #+cormanlisp "localhost" ;; is there a better way? Does it matter?
  #+allegro (symbol-call :excl.osi :gethostname)
  #+clisp (first (split-string (machine-instance) :separator " "))
  #+gcl (system:gethostname))

(defun* user-homedir ()
  (truenamize
   (pathname-directory-pathname
    #+cormanlisp (ensure-directory-pathname (user-homedir-pathname))
    #+mcl (current-user-homedir-pathname)
    #-(or cormanlisp mcl) (user-homedir-pathname))))

(defun* lisp-implementation-directory (&key truename)
  (let ((dir
          (ignore-errors
           #+clozure #p"ccl:"
           #+(or ecl mkcl) #p"SYS:"
           #+gcl system::*system-directory*
           #+sbcl (if-bind (it (find-symbol* :sbcl-homedir-pathname :sb-int nil))
                     (funcall it)
                     (getenv-pathname "SBCL_HOME" :want-directory t)))))
    (if (and dir truename)
        (truename* dir)
        dir)))


;;; Current directory

(defun* getcwd ()
  "Get the current working directory as per POSIX getcwd(3)"
  (or #+clisp (ext:default-directory)
      #+clozure (ccl:current-directory)
      #+cmu (unix:unix-current-directory)
      #+cormanlisp (pl::get-current-directory)
      #+ecl (ext:getcwd)
      #+mkcl (mk-ext:getcwd)
      #+sbcl (sb-unix:posix-getcwd/)
      (error "getcwd not supported on your implementation")))

(defun* chdir (x)
  "Change current directory, as per POSIX chdir(2)"
  #-(or clisp clozure) (when (pathnamep x) (setf x (native-namestring x)))
  (or #+clisp (ext:cd x)
      #+clozure (setf (ccl:current-directory) x)
      #+cormanlisp (unless (zerop (win32::_chdir x))
                     (error "Could not set current directory to ~A" x))
      #+sbcl (symbol-call :sb-posix :chdir x)
      (error "chdir not supported on your implementation")))

(defun* call-with-current-directory (dir thunk)
  (if dir
      (let* ((dir (truename (merge-pathnames (pathname-directory-pathname dir))))
             (*default-pathname-defaults* dir)
             (cwd (getcwd)))
        (chdir dir)
        (unwind-protect
             (funcall thunk)
          (chdir cwd)))
      (funcall thunk)))

(defmacro with-current-directory ((dir) &body body)
  "Call BODY while the POSIX current working directory is set to DIR"
  `(call-with-current-directory ,dir #'(lambda () ,@body)))


;;; Using temporary files

(defun* default-temporary-directory ()
  (flet ((f (s v d) (format nil "~A~A" (or (getenvp v) d (error "No temporary directory!")) s)))
    (let ((dir (cond
                 ((os-unix-p) (f #\/ "TMPDIR" "/tmp"))
                 ((os-windows-p) (f #\\ "TEMP" nil))))
          #+mcl (dir (probe-posix dir)))
      (or (parse-native-namestring dir) (default-directory)))))

(defvar *temporary-directory* nil)

(defun* temporary-directory ()
  (or *temporary-directory* (default-temporary-directory)))

(defun setup-temporary-directory ()
  (setf *temporary-directory* (default-temporary-directory)))

(defun* call-with-temporary-file
    (thunk &key
     prefix keep (direction :io)
     (element-type *default-stream-element-type*)
     (external-format :default))
  #+gcl<2.7 (declare (ignorable external-format))
  (check-type direction (member :output :io))
  (loop
    :with prefix = (or prefix (format nil "~Atmp" (native-namestring (temporary-directory))))
    :for counter :from (random (ash 1 32))
    :for pathname = (pathname (format nil "~A~36R" prefix counter)) :do
     ;; TODO: on Unix, do something about umask
     ;; TODO: on Unix, audit the code so we make sure it uses O_CREAT|O_EXCL
     ;; TODO: on Unix, use CFFI and mkstemp -- but the master is precisely meant to not depend on CFFI or on anything! Grrrr.
    (with-open-file (stream pathname
                            :direction direction
                            :element-type element-type
                            #-gcl<2.7 :external-format #-gcl<2.7 external-format
                            :if-exists nil :if-does-not-exist :create)
      (when stream
        (return
          (if keep
              (funcall thunk stream pathname)
              (unwind-protect
                   (funcall thunk stream pathname)
                (ignore-errors (delete-file pathname)))))))))

(defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                (pathname (gensym "PATHNAME") pathnamep)
                                prefix keep direction element-type external-format)
                               &body body)
  "Evaluate BODY where the symbols specified by keyword arguments
STREAM and PATHNAME are bound corresponding to a newly created temporary file
ready for I/O. Unless KEEP is specified, delete the file afterwards."
  (check-type stream symbol)
  (check-type pathname symbol)
  `(flet ((think (,stream ,pathname)
            ,@(unless pathnamep `((declare (ignore ,pathname))))
            ,@(unless streamp `((when ,stream (close ,stream))))
            ,@body))
     #-gcl (declare (dynamic-extent #'think))
     (call-with-temporary-file
      #'think
      ,@(when direction `(:direction ,direction))
      ,@(when prefix `(:prefix ,prefix))
      ,@(when keep `(:keep ,keep))
      ,@(when element-type `(:element-type ,element-type))
      ,@(when external-format `(:external-format external-format)))))

