;;;; ---------------------------------------------------------------------------
;;;; Utilities related to streams

(asdf/package:define-package :asdf/stream
  (:recycle :asdf/stream)
  (:use :cl :asdf/package :asdf/compatibility :asdf/utility)
  (:export
   #:*default-stream-element-type* #:*stderr*
   #:finish-outputs #:format!
   #:with-output #:with-input #:call-with-input-file
   #:with-safe-io-syntax #:read-function
   #:read-file-forms #:read-first-file-form
   #:copy-stream-to-stream #:concatenate-files
   #:copy-stream-to-stream-line-by-line
   #:slurp-stream-string #:slurp-stream-lines
   #:slurp-stream-forms #:slurp-file-string
   #:slurp-file-lines #:slurp-file-forms))
(in-package :asdf/stream)

(defvar *default-stream-element-type* (or #+(or abcl cmu cormanlisp scl xcl) 'character :default)
  "default element-type for open (depends on the current CL implementation)")

(defvar *stderr* #-clozure *error-output* #+clozure ccl::*stderr*
  "the original error output stream at startup")


;;; Ensure output buffers are flushed

(defun finish-outputs ()
  "Finish output on the main output streams.
Useful for portably flushing I/O before user input or program exit."
  ;; CCL notably buffers its stream output by default.
  (dolist (s (list *stderr* *error-output* *standard-output* *trace-output*))
    (ignore-errors (finish-output s)))
  (values))

(defun format! (stream format &rest args)
  "Just like format, but call finish-outputs before and after the output."
  (finish-outputs)
  (apply 'format stream format args)
  (finish-output stream))


;;; Output to a stream or string, FORMAT-style

(defgeneric call-with-output (x thunk)
  (:documentation
   ;; code from fare-utils base/streams where it's now named
   ;; call-with-output-stream to avoid the package clash in a lot of my code.
   "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error.")
  (:method ((x null) thunk)
    (declare (ignorable x))
    (with-output-to-string (s) (funcall thunk s)))
  (:method ((x (eql t)) thunk)
    (declare (ignorable x))
    (funcall thunk *standard-output*) nil)
  #-genera
  (:method ((x stream) thunk)
    (funcall thunk x) nil)
  (:method ((x string) thunk)
    (assert (fill-pointer x))
    (with-output-to-string (s x) (funcall thunk s)))
  (:method (x thunk)
    (declare (ignorable thunk))
    (cond
      #+genera
      ((typep x 'stream) (funcall thunk x) nil)
      (t (error "not a valid stream designator ~S" x)))))

(defmacro with-output ((x &optional (value x)) &body body)
  "Bind X to an output stream, coercing VALUE (default: previous binding of X)
as per FORMAT, and evaluate BODY within the scope of this binding."
  `(call-with-output ,value #'(lambda (,x) ,@body)))


;;; Input helpers

(defun call-with-input-file (pathname thunk
                             &key (element-type *default-stream-element-type*)
                             (external-format :default))
  "Open FILE for input with given options, call THUNK with the resulting stream."
  (with-open-file (s pathname :direction :input
                     :element-type element-type :external-format external-format
                     :if-does-not-exist :error)
    (funcall thunk s)))

(defmacro with-input-file ((var pathname &rest keys &key element-type external-format) &body body)
  (declare (ignore element-type external-format))
  `(call-with-input-file ,pathname #'(lambda (,var) ,@body) ,@keys))


;;; Reading helpers

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

(defun call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax ()
    (let ((*package* (find-package package))
          (*print-readably* nil)
	  (*read-eval* nil))
      (funcall thunk))))

(defun read-function (string)
  "Read a form from a string in function context, return a function"
  (eval `(function ,(read-from-string string))))

(defun* read-file-forms (file)
  (with-open-file (in file)
    (loop :with eof = (list nil)
     :for form = (read in nil eof)
     :until (eq form eof)
     :collect form)))

(defun read-first-file-form (pathname &key (package :cl) eof-error-p eof-value)
  "Reads the first form from the top of a file using a safe standardized syntax"
  (with-safe-io-syntax (:package package)
    (with-input-file (in pathname)
      (read in eof-error-p eof-value))))


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

(defun copy-stream-to-stream-line-by-line (input output &key prefix)
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

(defun slurp-stream-string (input &key (element-type 'character))
  "Read the contents of the INPUT stream as a string"
  (with-open-stream (input input)
    (with-output-to-string (output)
      (copy-stream-to-stream input output :element-type element-type))))

(defun slurp-stream-lines (input)
  "Read the contents of the INPUT stream as a list of lines"
  (with-open-stream (input input)
    (loop :for l = (read-line input nil nil) :while l :collect l)))

(defun slurp-stream-forms (input)
  "Read the contents of the INPUT stream as a list of forms"
  (with-open-stream (input input)
    (loop :with eof = '#:eof
      :for form = (read input nil eof)
      :until (eq form eof) :collect form)))

(defun slurp-file-string (file &rest keys)
  "Open FILE with option KEYS, read its contents as a string"
  (apply 'call-with-input-file file 'slurp-stream-string keys))

(defun slurp-file-lines (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of lines"
  (apply 'call-with-input-file file 'slurp-stream-lines keys))

(defun slurp-file-forms (file &rest keys)
  "Open FILE with option KEYS, read its contents as a list of forms"
  (apply 'call-with-input-file file 'slurp-stream-forms keys))

