(in-package :asdf-tools)

#|
Supported test lisp implementations include:
   allegro, abcl, ccl, clisp, cmucl, ecl, gcl, lispworks, mkcl, sbcl, scl and xcl.
Non supported are:
   cormancl, lispworks-personal-edition, mcl, genera

CCL is Clozure CL; (R)MCL is not supported.
Allegro is supported in the following flavors:
   allegro, allegro8, allegromodern, allegromodern8,
   allegro_s, allegro8_s, allegromodern_s, allegromodern8_s (SMP variants)
   allegro_64, allegro8_64, allegromodern_64, allegromodern8_64 (64-bit variants),
   allegro_64_s, allegro8_64_s, allegromodern_64_s, allegromodern8_64_s, (SMP, 64-bit variants)
Allegro CL is a special case: instead of setting environment variables for the specific runtime
locations, you may simply specify the Allegro install directories using these variables:
    ALLEGRO64DIR, ALLEGRO64SDIR (64-bit Allegro and SMP Allegro, respectively),
    ALLEGRODIR, and ALLEGROSDIR.

To configure the lisp implementations with which to run the tests,
you may export environment variables to override the defaults:
ASDF_TEST_LISPS and ASDF_UPGRADE_TEST_LISPS can each be a string of space-separated names
amongst the above implementation names.
You may also explicitly specify the same variables with the respective l= and u= arguments.
Individual test commands only use the first (preferred) provided implementation;
other test commands (named test-all-FOO) iterate over all implementations.

Similarly, you can configure which scripts to tests with ASDF_TEST_SCRIPTS or t=
and which systems to test loading with ASDF_TEST_SYSTEMS or s=

|#
(defun all-allegro-variants ()
  "Return a list of possible Allegro variants based on built-in information
and environment variables.  The returned list is made up of argument lists for
REGISTER-LISP-IMPLEMENTATION."
  (while-collecting (c)
    (loop :for (smpvar smpname smpfullname) :in `(("" "" "") ("S" :_s " (SMP)")) :do
      (loop
        :for (bitsvar bitsname bitsfullname) :in '(("" "" "") ("64" "_64" " (64-bit words)"))
        :for dirvar = (format nil "~:@(ALLEGRO~A~A~)" bitsvar smpvar)
        :for dir = (getenv-pathname dirvar :want-absolute t :ensure-directory t) :do
          (loop :for (charname charfullname) :in '(("" "") ("8" " (8-bit chars)")) :do
            (loop
              :for (caseexe casename casefullname) :in
              '(("a" "" "") ("m" :modern " (modern syntax)"))
              :for allegro-variant = (conc-keyword :allegro casename charname bitsname smpname)
              :for fullname = (strcat "Allegro CL"
                                      casefullname charfullname bitsfullname smpfullname)
              :for executable = (format nil "~(~alisp~a~)" caseexe charname) :do
                (c `(,allegro-variant
                     :fullname ,fullname
                     :name ,(native-namestring (subpathname dir executable))
                     :feature :allegro ;; do we want a more discriminating feature expression?
                     :flags '("-qq")
                     :eval-flag "-e"
                     :load-flag "-L"
                     ;; :quit-flags ("-kill")
                     :arguments-end "--"
                     :image-flag "-I"
                     :image-executable-p nil
                     :standalone-executable nil
                     :argument-control t
                     :disable-debugger ("-batch" ; see also -#D -#C -#!
                                        ,@(when (and (os-windows-p)
                                                     (not (getenvp "ALLEGRO_NOISY")))
                                            '("+c")))
                     :quit-format "(excl:exit ~A :quiet t)"
                     :dump-format "(progn (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) (excl:dumplisp :name ~A :suppress-allegro-cl-banner t))"))))))))

(defun register-lisp-implementation* (x)
  "Register the lisp implementation described a list X of a name followed by a plist"
  (apply 'register-lisp-implementation x))

(map () 'register-lisp-implementation* (all-allegro-variants))

(defun all-ecl-variants ()
  "Return a list of possible ECL variants with or withotu bytecode compiler"
  (loop
    :for (ecl-variant extraname flags) :in
    '((:ecl "" ("-load" "sys:cmp"))
      (:ecl_bytecodes " (using bytecodes compiler)" ("-eval" "(ext::install-bytecodes-compiler)")))
    :for fullname = (strcat "Embeddable Common-Lisp" extraname) :collect
    `(,ecl-variant
      :fullname ,fullname
      :name "ecl"
      :feature :ecl
      :environment-variable "ECL"
      :flags ("-norc" ,@flags)
      :eval-flag "-eval" ; -e
      :load-flag "-load"
      :image-flag nil
      :image-executable-p t
      :arguments-end "--"
      :argument-control t ;; must be fixed now, but double-checking needed.
      :disable-debugger ()
      :quit-format "(si:quit ~A)"
      :dump-format nil))) ;; Cannot dump with ECL. Link instead.

(map () 'register-lisp-implementation* (all-ecl-variants))

(defun acceptable-eval-character-p (character)
  "Is this character acceptable in an eval form, even when used under Windows?"
  (let ((code (char-code character)))
    (and (< 32 code 127) (not (find character "'\\\"")))))

(defun acceptable-eval-symbol-character-p (character)
  "Is this character acceptable as part of a symbol in an eval form, even when used under Windows?"
  (let ((code (char-code character))) ;; assumes uppercase.
    ;; No "modern" syntax accepted on the test master, though it can be used on the slave.
    (and (< 32 code 91) (not (find character "'\\\":;")))))

(defun need-escape-within-pipe-p (character)
  "Does this character need to be escaped when within symbol pipe notation?"
  (or (eql character #\|) (eql character #\\)))

(defun print-string-in-pipes (string &optional s)
  "Print a symbol |escaped| between pipes with the given STRING as its name, to the output S"
  (with-output (s)
    (princ "|" s)
    (let ((string (string string)))
      (if (some 'need-escape-within-pipe-p string)
          (loop :for c :across string :do
                (when (need-escape-within-pipe-p c) (princ #\\ s))
                (princ c s))
          (princ string s)))
    (princ "|" s)))

(defun compose-eval-string (forms &optional s)
  "Given a some FORMS, compose a string suitable to be passed for evaluation
to a Lisp implementation from the shell command-line with e.g. sbcl --eval or equivalent.
Output S is as specified as per WITH-OUTPUT.
FORMS may be a preformatted string, or a list of forms, where each form is either
a preformatted string, or a LIST that specifies a program."
  (etypecase forms
    ;; at the toplevel, a string mean "this is pre-formatted", you know what to pass to --eval
    (string
     (output-string forms s))
    (list
     (with-standard-io-syntax
       (let ((*package* (find-package :cl)))
         (with-output (s)
           (labels
               ((p (x) (princ x s)) ;; princ
                (w (x) (write x :stream s :case :downcase)) ;; write
                (c (x) ;; form that evaluates into a character
                  (p "(code-char`") (p (char-code x)) (p ")"))
                (sym (x) ;; print a symbol
                  ;; check that only good characters are used in a symbol;
                  ;; TODO: if not, go through a string (ugh!)
                  (assert (every 'acceptable-eval-symbol-character-p (package-name (symbol-package x))))
                  (assert (every 'acceptable-eval-symbol-character-p (symbol-name x)))
                  (w x))
                (s (x) ;; form that evaluates into a string
                   (if (every 'acceptable-eval-character-p x)
                       (progn ;; simple string literal
                         (p "(string`") (print-string-in-pipes x s) (p ")"))
                       (progn
                         (p "(format()(string`|~{~a~}|)`(")
                         (loop
                           :with end = (length x)
                           :for start = 0 :then (if position (1+ position) end)
                           :for morep = (< start end)
                           :for position = (and morep
                                                (position-if-not 'acceptable-eval-character-p x
                                                                 :start start))
                           :while morep
                           :do (when (or (null position) (< start position))
                                 (print-string-in-pipes (subseq x start position) s))
                               (when position
                                 (p ",") (c (char x position))))
                         (p "))"))))
                (n (x) ;; is it the easy case where no space is needed in the list?
                  (loop :for (a . d) :on x :do
                    (cond
                      ((null d) (return t))
                      ((atom d) (return nil))
                      ((or (typep a '(or null character string cons))
                           (and (typep (car d) 'list) (n (car d)))))
                      (t (return nil)))))
                (f (x) ;; top-level function to print a form without spaces, ' " \
                  (etypecase x
                    (null (p "()"))
                    (symbol (sym x))
                    (real (w x))
                    (character (p "#.") (c x))
                    (string (progn (p "#.") (s x)))
                    (cons
                     (if (n x)
                         (progn (p "(") (map () #'f x) (p ")"))
                         (progn (p "#.`") (b x))))))
                (b (x) ;; print a form that, when inside a backquote, evaluates to the form we want
                  (etypecase x
                    (null (p "()"))
                    (symbol (sym x))
                    (real (w x))
                    (character (p ",") (c x))
                    (string (progn (p ",") (s x)))
                    (cons
                     (p "(")
                     (loop :for (a . d) :on x :do
                       (b a)
                       (cond
                         ((null d)) ;; Done: d is a close paren
                         ((typep a '(or null character string cons))) ;; Done: a ends with )
                         ((atom d)
                          (p ",@") ;; NB: we rely on ,@ being the same as ., in practice
                          (typecase d
                            (character (c d))
                            (string (s d))
                            (real (w d))
                            ((or keyword (eql t)) (sym d))
                            (t (p "`") (b d))))
                         ((typep (car d) '(or list character string))) ;; Done: d starts with , or (
                         ((typep (car d) '(or real keyword boolean)) ;; insert a , before constant
                          (p ","))
                         ((typep (car d) 'symbol)
                          (p ",`")) ;; insert a ,` before variable symbol
                         (t (error "foo"))))
                     (p ")")))))
             (p "(quote(")
             (loop :for form :in forms :do
               (p "#.")
               (etypecase form
                 (string (p form)) ;; forms directly under the top-level can be preformatted, too
                 (list (f form))))
             (p "))"))))))))

(defun compose-copy-paste-string (forms &optional s)
  "Given FORMS to execute, specified as either a S-expression or a pre-formatted string,
output to S some text without spaces or shell-special characters that will evaluate
to the same form"
  (etypecase forms
    (string (output-string forms s))
    (list (format s "~{~A~%~}"
                  (with-standard-io-syntax
                    (let ((*package* (find-package :cl))
                          (*print-case* :downcase))
                      (mapcar (lambda (x)
                                (typecase x (string x)
                                          (t (write-to-string x))))
                              forms)))))))
