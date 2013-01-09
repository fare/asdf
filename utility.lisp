;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities for ASDF

(asdf/package:define-package :asdf/utility
  (:recycle :asdf/utility :asdf)
  (:use :common-lisp :asdf/package :asdf/implementation)
  (:export
   #:find-symbol* ;; reexport from asdf/package
   #:strcat #:compatfmt ;; reexport from asdf/implementation
   #:defun* #:defgeneric* ;; defining macros
   #:aif #:it ;; basic flow control
   #:while-collecting #:appendf #:length=n-p ;; lists
   #:remove-keys #:remove-keyword ;; keyword argument lists
   #:first-char #:last-char #:split-string #:string-suffix-p ;; strings
   #:find-class* ;; CLOS
   #:stamp< #:stamp<= #:earlier-stamp #:stamps-earliest #:earliest-stamp ;; stamps
   #:later-stamp #:stamps-latest #:latest-stamp #:latest-stamp-f
   #:list-to-hash-set ;; hash-table
   #:ensure-function ;; functions
   #:parse-version #:version-compatible-p)) ;; version
(in-package :asdf/utility)

;;; *-defining macros

(macrolet
    ((defdef (def* def)
       `(defmacro ,def* (name formals &rest rest)
          `(progn
             #+(or ecl gcl)
             ,(when (and #+gcl<2.7 (symbolp name))
                `(fmakunbound ',name))
             #-gcl ; gcl 2.7.0 notinline functions lose secondary return values :-(
             ,(when (and #+ecl (symbolp name)) ; fails for setf functions on ecl
                `(declaim (notinline ,name)))
             (,',def ,name ,formals ,@rest)))))
  (defdef defgeneric* defgeneric)
  (defdef defun* defun))

;;; Flow control
(defmacro aif (test then &optional else)
  "Anaphoric version of IF, On Lisp style"
  `(let ((it ,test)) (if it ,then ,else)))

;;; List manipulation
(defmacro while-collecting ((&rest collectors) &body body)
  "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

(define-modify-macro appendf (&rest args)
  append "Append onto list") ;; only to be used on short lists.

(defun* length=n-p (x n) ;is it that (= (length x) n) ?
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

;;; Keyword argument lists
(defun* remove-keys (key-names args)
  (loop :for (name val) :on args :by #'cddr
    :unless (member (symbol-name name) key-names
                    :key #'symbol-name :test 'equal)
    :append (list name val)))

(defun* remove-keyword (key args)
  (loop :for (k v) :on args :by #'cddr
    :unless (eq k key)
    :append (list k v)))

;;; Strings
(defun* first-char (s)
  (and (stringp s) (plusp (length s)) (char s 0)))

(defun* last-char (s)
  (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

(defun* split-string (string &key max (separator '(#\Space #\Tab)))
  "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
  (catch nil
    (let ((list nil) (words 0) (end (length string)))
      (flet ((separatorp (char) (find char separator))
             (done () (throw nil (cons (subseq string 0 end) list))))
        (loop
          :for start = (if (and max (>= words (1- max)))
                           (done)
                           (position-if #'separatorp string :end end :from-end t)) :do
          (when (null start)
            (done))
          (push (subseq string (1+ start) end) list)
          (incf words)
          (setf end start))))))

(defun* string-suffix-p (s suffix)
  (check-type s string)
  (check-type suffix string)
  (let ((start (- (length s) (length suffix))))
    (and (<= 0 start)
         (string-equal s suffix :start1 start))))

;;; CLOS
(defun* find-class* (x &optional (errorp t) environment)
  (etypecase x
    ((or standard-class built-in-class) x)
    #+gcl<2.7 (keyword nil)
    (symbol (find-class x errorp environment))))

;;; stamps: real or boolean where NIL=-infinity, T=+infinity
(defun* stamp< (x y)
  (etypecase x
    (null (and y t))
    ((eql t) nil)
    (real (etypecase y
            (null nil)
            ((eql t) t)
            (real (< x y))))))
;;(defun* stamps< (list) (loop :for y :in list :for x = nil :then y :always (stamp< x y)))
;;(defun* stamp*< (&rest list) (stamps< list))
(defun* stamp<= (x y) (not (stamp< y x)))
(defun* earlier-stamp (x y) (if (stamp< x y) x y))
(defun* stamps-earliest (list) (reduce 'earlier-stamp list :initial-value t))
(defun* earliest-stamp (&rest list) (stamps-earliest list))
(defun* later-stamp (x y) (if (stamp< x y) y x))
(defun* stamps-latest (list) (reduce 'later-stamp list :initial-value nil))
(defun* latest-stamp (&rest list) (stamps-latest list))
(define-modify-macro latest-stamp-f (&rest stamps) latest-stamp)

;;; Hash-tables
(defun* list-to-hash-set (list &aux (h (make-hash-table :test 'equal)))
  (dolist (x list h) (setf (gethash x h) t)))

;;; Functions
(defun* ensure-function (fun &key (package :asdf))
  (etypecase fun
    ((or boolean keyword character number pathname) (constantly fun))
    ((or function symbol) fun)
    (cons (eval `(function ,fun)))
    (string (eval `(function ,(with-standard-io-syntax
                                (let ((*package* (find-package package)))
                                  (read-from-string fun))))))))


;;; Version handling
(defun* parse-version (string &optional on-error)
  "Parse a version string as a series of natural integers separated by dots.
Return a (non-null) list of integers if the string is valid, NIL otherwise.
If on-error is error, warn, or designates a function of compatible signature,
the function is called with an explanation of what is wrong with the argument.
NB: ignores leading zeroes, and so doesn't distinguish between 2.003 and 2.3"
  (and
   (or (stringp string)
       (when on-error
         (funcall on-error "~S: ~S is not a string"
                  'parse-version string)) nil)
   (or (loop :for prev = nil :then c :for c :across string
         :always (or (digit-char-p c)
                     (and (eql c #\.) prev (not (eql prev #\.))))
         :finally (return (and c (digit-char-p c))))
       (when on-error
         (funcall on-error "~S: ~S doesn't follow asdf version numbering convention"
                  'parse-version string)) nil)
   (mapcar #'parse-integer (split-string string :separator "."))))

(defun* version-compatible-p (provided-version required-version)
  "Is the provided version a compatible substitution for the required-version?
If major versions differ, it's not compatible.
If they are equal, then any later version is compatible,
with later being determined by a lexicographical comparison of minor numbers."
  (let ((x (parse-version provided-version 'warn))
        (y (parse-version required-version 'warn)))
    (labels ((bigger (x y)
               (cond ((not y) t)
                     ((not x) nil)
                     ((> (car x) (car y)) t)
                     ((= (car x) (car y))
                      (bigger (cdr x) (cdr y))))))
      (and x y (= (car x) (car y))
           (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

