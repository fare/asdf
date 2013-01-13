;;;; ---------------------------------------------------------------------------
;;;; Utilities related to streams

(asdf/package:define-package :asdf/stream
  (:recycle :asdf/stream)
  (:use :cl :asdf/package :asdf/compatibility :asdf/utility :asdf/pathname)
  (:export
   #:*default-stream-element-type* #:*stderr* #:setup-stderr
   #:with-safe-io-syntax #:call-with-safe-io-syntax
   #:with-output #:output-string #:with-input
   #:with-input-file #:call-with-input-file
   #:finish-outputs #:format! #:safe-format!
   #:read-file-forms #:read-first-file-form
   #:copy-stream-to-stream #:concatenate-files
   #:copy-stream-to-stream-line-by-line
   #:slurp-stream-string #:slurp-stream-lines
   #:slurp-stream-forms #:read-file-string
   #:read-file-lines #:read-file-forms
   #:safe-read-first-file-form #:eval-input
   #:detect-encoding #:*encoding-detection-hook* #:always-default-encoding
   #:encoding-external-format #:*encoding-external-format-hook* #:default-encoding-external-format
   #:*default-encoding* #:*utf-8-external-format*))
(in-package :asdf/stream)

(defvar *default-stream-element-type* (or #+(or abcl cmu cormanlisp scl xcl) 'character :default)
  "default element-type for open (depends on the current CL implementation)")

(defvar *stderr* #-clozure *error-output* #+clozure ccl::*stderr*
  "the original error output stream at startup")

(defun setup-stderr ()
  (setf *stderr* #-clozure *error-output* #+clozure ccl::*stderr*))


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

(defun* call-with-output (x thunk)
  "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error."
  (typecase x
    (null
     (with-output-to-string (s) (funcall thunk s)))
    ((eql t)
     (funcall thunk *standard-output*))
    (stream
     (funcall thunk x))
    (string
     (assert (fill-pointer x))
     (with-output-to-string (s x) (funcall thunk s)))
    (t (error "not a valid stream designator ~S" x))))

(defmacro with-output ((x &optional (value x)) &body body)
  "Bind X to an output stream, coercing VALUE (default: previous binding of X)
as per FORMAT, and evaluate BODY within the scope of this binding."
  `(call-with-output ,value #'(lambda (,x) ,@body)))

(defun* output-string (string &optional stream)
  (if stream
      (with-output (stream) (princ string stream))
      string))


;;; Input helpers

(defun* call-with-input (x fun)
  "Calls FUN with an actual stream argument, coercing behaving like READ with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error."
  (typecase x
    (null
     (funcall fun *terminal-io*))
    ((eql t)
     (funcall fun *standard-input*))
    (stream
     (funcall fun x))
    (string
     (with-input-from-string (s x) (funcall fun s)))
    (t
     (error "not a valid input stream designator ~S" x))))

(defmacro with-input ((x &optional (value x)) &body body)
  `(call-with-input ,value #'(lambda (,x) ,@body)))

(defun* call-with-input-file (pathname thunk
                             &key (element-type *default-stream-element-type*)
                             (external-format :default))
  "Open FILE for input with given options, call THUNK with the resulting stream."
  #+gcl<2.7 (declare (ignore external-format))
  (with-open-file (s pathname :direction :input
                     :element-type element-type
                     #-gcl<2.7 :external-format #-gcl<2.7 external-format
                     :if-does-not-exist :error)
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
                     (list *stderr* *error-output* *standard-output* *trace-output* *debug-io*)))
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

(defun* slurp-stream-lines (input)
  "Read the contents of the INPUT stream as a list of lines"
  (with-open-stream (input input)
    (loop :for l = (read-line input nil nil) :while l :collect l)))

(defun* slurp-stream-forms (input)
  "Read the contents of the INPUT stream as a list of forms.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (with-open-stream (input input)
    (loop :with eof = '#:eof
      :for form = (read input nil eof)
      :until (eq form eof) :collect form)))

(defun* read-file-string (file &rest keys)
  "Open FILE with option KEYS, read its contents as a string"
  (apply 'call-with-input-file file 'slurp-stream-string keys))

(defun* read-file-lines (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of lines
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (apply 'call-with-input-file file 'slurp-stream-lines keys))

(defun* read-file-forms (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of forms.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (apply 'call-with-input-file file 'slurp-stream-forms keys))

(defun* read-first-file-form (pathname &key eof-error-p eof-value)
  "Reads the first form from the top of a file.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (with-input-file (in pathname)
    (read-preserving-whitespace in eof-error-p eof-value)))

(defun* safe-read-first-file-form (pathname &key
                                            (package :cl)
                                            eof-error-p eof-value)
  "Reads the first form from the top of a file using a safe standardized syntax"
  (with-safe-io-syntax (:package package)
    (read-first-file-form pathname :eof-error-p eof-error-p :eof-value eof-value)))

(defun* eval-input (input)
  "Portably read and evaluate forms from INPUT, return the last values."
  (with-input (input)
    (loop :with results :with eof ='#:eof
          :for form = (read input nil eof)
          :until (eq form eof)
          :do (setf results (multiple-value-list (eval form)))
          :finally (return (apply 'values results)))))


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

