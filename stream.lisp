;;;; ---------------------------------------------------------------------------
;;;; Utilities related to streams

(asdf/package:define-package :asdf/stream
  (:recycle :asdf/stream)
  (:use :cl :asdf/package :asdf/compatibility :asdf/utility :asdf/pathname)
  #+gcl<2.7 (:shadowing-import-from :asdf/compatibility #:with-standard-io-syntax)
  (:export
   #:*default-stream-element-type* #:*stderr* #:setup-stderr
   #:with-safe-io-syntax #:call-with-safe-io-syntax
   #:with-output #:output-string #:with-input
   #:with-input-file #:call-with-input-file
   #:finish-outputs #:format! #:safe-format!
   #:copy-stream-to-stream #:concatenate-files
   #:copy-stream-to-stream-line-by-line
   #:slurp-stream-string #:slurp-stream-lines #:slurp-stream-forms #:slurp-stream-form
   #:read-file-string #:read-file-lines #:read-file-forms #:read-file-form #:safe-read-file-form
   #:eval-input #:eval-thunk #:standard-eval-thunk
   #:detect-encoding #:*encoding-detection-hook* #:always-default-encoding
   #:encoding-external-format #:*encoding-external-format-hook* #:default-encoding-external-format
   #:*default-encoding* #:*utf-8-external-format*))
(in-package :asdf/stream)

(defvar *default-stream-element-type* (or #+(or abcl cmu cormanlisp scl xcl) 'character :default)
  "default element-type for open (depends on the current CL implementation)")

(defvar *stderr* *error-output*
  "the original error output stream at startup")

(defun setup-stderr ()
  (setf *stderr*
        #+allegro excl::*stderr*
        #+clozure ccl::*stderr*
        #-(or allegro clozure) *error-output*))
(setup-stderr)


;;; Safe syntax

(defvar *standard-readtable* (copy-readtable nil))

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

(defun* call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
          (*readtable* *standard-readtable*)
          (*read-default-float-format* 'double-float)
          (*print-readably* nil)
	  (*read-eval* nil))
      (funcall thunk))))


;;; Output to a stream or string, FORMAT-style

(defun* call-with-output (output function)
  "Calls FUNCTION with an actual stream argument,
behaving like FORMAT with respect to how stream designators are interpreted:
If OUTPUT is a stream, use it as the stream.
If OUTPUT is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OUTPUT is T, use *STANDARD-OUTPUT* as the stream.
If OUTPUT is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error."
  (etypecase output
    (null
     (with-output-to-string (stream) (funcall function stream)))
    ((eql t)
     (funcall function *standard-output*))
    (stream
     (funcall function output))
    (string
     (assert (fill-pointer output))
     (with-output-to-string (stream output) (funcall function stream)))))

(defmacro with-output ((output-var &optional (value output-var)) &body body)
  "Bind OUTPUT-VAR to an output stream, coercing VALUE (default: previous binding of OUTPUT-VAR)
as per FORMAT, and evaluate BODY within the scope of this binding."
  `(call-with-output ,value #'(lambda (,output-var) ,@body)))

(defun* output-string (string &optional output)
  "If the desired OUTPUT is not NIL, print the string to the output; otherwise return the string"
  (if output
      (with-output (output) (princ string output))
      string))


;;; Input helpers

(defun* call-with-input (input function)
  "Calls FUNCTION with an actual stream argument, interpreting
stream designators like READ, but also coercing strings to STRING-INPUT-STREAM.
If INPUT is a STREAM, use it as the stream.
If INPUT is NIL, use a *STANDARD-INPUT* as the stream.
If INPUT is T, use *TERMINAL-IO* as the stream.
As an extension, if INPUT is a string, use it as a string-input-stream.
Otherwise, signal an error."
  (etypecase input
    (null (funcall function *standard-input*))
    ((eql t) (funcall function *terminal-io*))
    (stream (funcall function input))
    (string (with-input-from-string (stream input) (funcall function stream)))))

(defmacro with-input ((input-var &optional (value input-var)) &body body)
  "Bind INPUT-VAR to an input stream, coercing VALUE (default: previous binding of INPUT-VAR)
as per CALL-WITH-INPUT, and evaluate BODY within the scope of this binding."
  `(call-with-input ,value #'(lambda (,input-var) ,@body)))

(defun* call-with-input-file (pathname thunk
                                       &key
                                       (element-type *default-stream-element-type*)
                                       (external-format :default)
                                       (if-does-not-exist :error))
  "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
  #+gcl<2.7 (declare (ignore external-format))
  (with-open-file (s pathname :direction :input
                     :element-type element-type
                     #-gcl<2.7 :external-format #-gcl<2.7 external-format
                     :if-does-not-exist if-does-not-exist)
    (funcall thunk s)))

(defmacro with-input-file ((var pathname &rest keys &key element-type external-format) &body body)
  (declare (ignore element-type external-format))
  `(call-with-input-file ,pathname #'(lambda (,var) ,@body) ,@keys))


;;; Ensure output buffers are flushed

(defun* finish-outputs (&rest streams)
  "Finish output on the main output streams as well as any specified one.
Useful for portably flushing I/O before user input or program exit."
  ;; CCL notably buffers its stream output by default.
  (dolist (s (append streams
                     (list *stderr* *error-output* *standard-output* *trace-output*
                           *debug-io* *terminal-io* *debug-io* *query-io*)))
    (ignore-errors (finish-output s)))
  (values))

(defun* format! (stream format &rest args)
  "Just like format, but call finish-outputs before and after the output."
  (finish-outputs stream)
  (apply 'format stream format args)
  (finish-output stream))

(defun* safe-format! (stream format &rest args)
  (with-safe-io-syntax ()
    (ignore-errors (apply 'format! stream format args))
    (finish-outputs stream))) ; just in case format failed


;;; Simple Whole-Stream processing

(defun* copy-stream-to-stream (input output &key (element-type 'character) (buffer-size 8192))
  "Copy the contents of the INPUT stream into the OUTPUT stream,
using WRITE-SEQUENCE and a sensibly sized buffer."
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

(defun* copy-stream-to-stream-line-by-line (input output &key prefix)
  "Copy the contents of the INPUT stream into the OUTPUT stream,
reading contents line by line."
  (with-open-stream (input input)
    (loop :for (line eof) = (multiple-value-list (read-line input nil nil))
      :while line :do
      (when prefix (princ prefix output))
      (princ line output)
      (unless eof (terpri output))
      (finish-output output)
      (when eof (return)))))

(defun* slurp-stream-string (input &key (element-type 'character))
  "Read the contents of the INPUT stream as a string"
  (with-open-stream (input input)
    (with-output-to-string (output)
      (copy-stream-to-stream input output :element-type element-type))))

(defun* slurp-stream-lines (input &key count)
  "Read the contents of the INPUT stream as a list of lines, return those lines.

Read no more than COUNT lines."
  (check-type count (or null integer))
  (with-open-stream (input input)
    (loop :for n :from 0
          :for l = (and (or (not count) (< n count))
                        (read-line input nil nil))
          :while l :collect l)))

(defun* slurp-stream-line (input &key (path 0))
  "Read the contents of the INPUT stream as a list of lines,
then return the SUB-OBJECT of that list of lines following the PATH.
PATH defaults to 0, i.e. return the first line.
PATH is typically an integer, or a list of an integer and a function.
If PATH is NIL, it will return all the lines in the file.

The stream will not be read beyond the Nth lines,
where N is the index specified by path
if path is either an integer or a list that starts with an integer."
  (let* ((count (cond
                  ((integerp path)
                   (1+ path))
                  ((and (consp path) (integerp (first path)))
                   (1+ (first path)))))
         (forms (slurp-stream-lines input :count count)))
    (sub-object forms path)))

(defun* slurp-stream-forms (input &key count)
"Read the contents of the INPUT stream as a list of forms,
and return those forms.

If COUNT is null, read to the end of the stream;
if COUNT is an integer, stop after COUNT forms were read.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (check-type count (or null integer))
  (loop :with eof = '#:eof
        :for n :from 0
        :for form = (if (and count (>= n count))
                        eof
                        (read-preserving-whitespace input nil eof))
        :until (eq form eof) :collect form))

(defun* slurp-stream-form (input &key (path 0))
"Read the contents of the INPUT stream as a list of forms,
then return the SUB-OBJECT of these forms following the PATH.
PATH defaults to 0, i.e. return the first form.
PATH is typically a list of integers.
If PATH is NIL, it will return all the forms in the file.

The stream will not be read beyond the Nth form,
where N is the index specified by path,
if path is either an integer or a list that starts with an integer.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (let* ((count (cond
                  ((integerp path)
                   (1+ path))
                  ((and (consp path) (integerp (first path)))
                   (1+ (first path)))))
         (forms (slurp-stream-forms input :count count)))
    (sub-object forms path)))

(defun* read-file-string (file &rest keys)
  "Open FILE with option KEYS, read its contents as a string"
  (apply 'call-with-input-file file 'slurp-stream-string keys))

(defun* read-file-lines (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of lines
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (apply 'call-with-input-file file 'slurp-stream-lines keys))

(defun* read-file-forms (file &rest keys &key count &allow-other-keys)
  "Open input FILE with option KEYS (except COUNT),
and read its contents as per SLURP-STREAM-FORMS with given COUNT.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (apply 'call-with-input-file file
         #'(lambda (input) (slurp-stream-forms input :count count))
         (remove-plist-key :count keys)))

(defun* read-file-form (file &rest keys &key (path 0) &allow-other-keys)
  "Open input FILE with option KEYS (except path),
and read its contents as per SLURP-STREAM-FORM with given PATH.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (apply 'call-with-input-file file
         #'(lambda (input) (slurp-stream-form input :path path))
         (remove-plist-key :path keys)))

(defun* safe-read-file-form (pathname &rest keys &key (package :cl) &allow-other-keys)
  "Reads the specified form from the top of a file using a safe standardized syntax.
Extracts the form using READ-FILE-FORM,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
  (with-safe-io-syntax (:package package)
    (apply 'read-file-form pathname (remove-plist-key :package keys))))

(defun* eval-input (input)
  "Portably read and evaluate forms from INPUT, return the last values."
  (with-input (input)
    (loop :with results :with eof ='#:eof
          :for form = (read input nil eof)
          :until (eq form eof)
          :do (setf results (multiple-value-list (eval form)))
          :finally (return (apply 'values results)))))

(defun* eval-thunk (thunk)
  "Evaluate a THUNK of code:
If a function, FUNCALL it without arguments.
If a constant literal and not a sequence, return it.
If a cons or a symbol, EVAL it.
If a string, repeatedly read and evaluate from it, returning the last values."
  (etypecase thunk
    ((or boolean keyword number character pathname) thunk)
    ((or cons symbol) (eval thunk))
    (function (funcall thunk))
    (string (eval-input thunk))))

(defun* standard-eval-thunk (thunk &key (package :cl))
  "Like EVAL-THUNK, but in a more standardized evaluation context."
  ;; Note: it's "standard-" not "safe-", because evaluation is never safe.
  (when thunk
    (with-safe-io-syntax (:package package)
      (let ((*read-eval* t))
        (eval-thunk thunk)))))


;;; Encodings

(defvar *default-encoding* :default
  "Default encoding for source files.
The default value :default preserves the legacy behavior.
A future default might be :utf-8 or :autodetect
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.")

(defparameter *utf-8-external-format*
  #+(and asdf-unicode (not clisp)) :utf-8
  #+(and asdf-unicode clisp) charset:utf-8
  #-asdf-unicode :default
  "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

(defun* always-default-encoding (pathname)
  (declare (ignore pathname))
  *default-encoding*)

(defvar *encoding-detection-hook* #'always-default-encoding
  "Hook for an extension to define a function to automatically detect a file's encoding")

(defun* detect-encoding (pathname)
  (if (and pathname (not (directory-pathname-p pathname)) (probe-file pathname))
      (funcall *encoding-detection-hook* pathname)
      *default-encoding*))

(defun* default-encoding-external-format (encoding)
  (case encoding
    (:default :default) ;; for backward-compatibility only. Explicit usage discouraged.
    (:utf-8 *utf-8-external-format*)
    (otherwise
     (cerror "Continue using :external-format :default" (compatfmt "~@<Your ASDF component is using encoding ~S but it isn't recognized. Your system should :defsystem-depends-on (:asdf-encodings).~:>") encoding)
     :default)))

(defvar *encoding-external-format-hook*
  #'default-encoding-external-format
  "Hook for an extension to define a mapping between non-default encodings
and implementation-defined external-format's")

(defun* encoding-external-format (encoding)
  (funcall *encoding-external-format-hook* encoding))

