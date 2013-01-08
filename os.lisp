;;;; ---------------------------------------------------------------------------
;;;; Access to the Operating System 
(defpackage :asdf/os
  (:use :cl :asdf/package :asdf/implementation :asdf/utility :asdf/pathname)
  (:export
   #:featurep #:os-unix-p #:os-windows-p ;; features
   #:read-file-forms ;; simple filesystem manipulation
   #:copy-stream-to-stream #:concatenate-files ;; simple stream copy
   #:getenv ;; environment variables, and parsing them
   #:inter-directory-separator #:split-pathnames*
   #:getenv-pathname #:getenv-pathnames
   #:getenv-absolute-directory #:getenv-absolute-directories
   #:implementation-identifier ;; implementation identifier
   #:implementation-type #:operating-system #:architecture #:lisp-version-string
   #:hostname #:user-homedir #:lisp-implementation-directory))
(in-package :asdf/os)

;;; Features
(defun* featurep (x &optional (features *features*))
  (cond
    ((atom x)
     (and (member x features) t))
    ((eq :not (car x))
     (assert (null (cddr x)))
     (not (featurep (cadr x) features)))
    ((eq :or (car x))
     (some #'(lambda (x) (featurep x features)) (cdr x)))
    ((eq :and (car x))
     (every #'(lambda (x) (featurep x features)) (cdr x)))
    (t
     (error "Malformed feature specification ~S" x))))

(defun* os-unix-p ()
  (featurep '(:or :unix :cygwin :darwin)))

(defun* os-windows-p ()
  (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32))))

;;; Simple filesystem manipulation
(defun* read-file-forms (file)
  (with-open-file (in file)
    (loop :with eof = (list nil)
     :for form = (read in nil eof)
     :until (eq form eof)
     :collect form)))

;; Simple stream copy
(defun* copy-stream-to-stream (input output &key (element-type 'character) (buffer-size 8192))
  "Copy the contents of the INPUT stream into the OUTPUT stream,
using WRITE-SEQUENCE and a sensibly sized buffer." ; copied from xcvb-driver
  (with-open-stream (input input)
    (loop
      :for buffer = (make-array (list buffer-size) :element-type element-type)
      :for end = (read-sequence buffer input)
      :until (zerop end)
      :do (write-sequence buffer output :end end)
          (when (< end buffer-size) (return)))))

(defun* concatenate-files (inputs output)
  (with-open-file (o output :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :rename-and-delete)
    (dolist (input inputs)
      (with-open-file (i input :element-type '(unsigned-byte 8)
                               :direction :input :if-does-not-exist :error)
        (copy-stream-to-stream i o :element-type '(unsigned-byte 8))))))


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
  #+mkcl (#.(or (find-symbol* 'getenv :si) (find-symbol* 'getenv :mk-ext)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S is not supported on your implementation" 'getenv))

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

(defun* first-feature (features)
  (dolist (x features)
    (multiple-value-bind (val feature)
        (if (consp x) (values (first x) (cons :or (rest x))) (values x x))
      (when (featurep feature) (return val)))))

(defun* implementation-type ()
  (first-feature
   '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp) :cmu
     :ecl :gcl (:lw :lispworks) :mcl :mkcl :sbcl :scl :symbolics :xcl)))

(defun* operating-system ()
  (first-feature
   '(:cygwin (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
     (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
     (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
     (:solaris :solaris :sunos) (:bsd :bsd :freebsd :netbsd :openbsd) :unix
     :genera)))

(defun* architecture ()
  (first-feature
   '((:x64 :amd64 :x86-64 :x86_64 :x8664-target (:and :word-size=64 :pc386))
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
  #+allegro (excl.osi:gethostname)
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
           #+clozure (let ((*default-pathname-defaults* #p"")) (truename #p"ccl:"))
           #+(or ecl mkcl) #p"SYS:"
           #+sbcl (aif (find-symbol* :sbcl-homedir-pathname :sb-int)
                       (funcall it)
                       (getenv-pathname "SBCL_HOME" :want-directory t)))))
    (if (and dir truename)
        (let ((*default-pathname-defaults* #p"")) (truename dir))
        dir)))

