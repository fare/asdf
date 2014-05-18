;;; The code below exemplifies how to load and configure ASDF
;;; as part of your own deterministic build.
;;; See "User-configurable parts" for where you'd customize it to suit your build.
;;;
;;; We have to play games with packages because on some implementations,
;;; an ASDF upgrade from ASDF 2 can throw away the previous package.
;;;
;;; Everything is MUCH simpler if you can assume your implementation has a recent-enough ASDF 3:
;;; see the commented out alternative below.
;;;
;;; To use the user-configured ASDF rather than a deterministic self-contained project build,
;;; see instead how cl-launch 4.0.4 loads ASDF.
;;; Actually, if you can assume that your implementation or distribution provides ASDF 3,
;;; you may simply use cl-launch, and achieve a deterministic self-contained project build
;;; by having your shell configuration or some shell wrapper script export a proper values
;;; for the CL_SOURCE_REGISTRY and ASDF_OUTPUT_TRANSLATIONS environment variables.

(in-package :cl-user) ;; That may be default, but let's make double sure and tell SLIME.

;; Do everything in eval-when, so this works
;; whether this file is being loaded directly or compiled first.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((asdf-symbol (name)
             (and (find-package :asdf) (find-symbol (string name) :asdf)))
           (asdf-call (name &rest args)
             (apply (asdf-symbol name) args))
           (asdf-version ()
             (when (find-package :asdf)
               (or (symbol-value (or (asdf-symbol '*asdf-version*)
                                     (asdf-symbol '*asdf-revision*)))
                   "1.0")))
           (subpath (parent &key directory name type version)
             ;; We need subpath here, because we can't yet assume ASDF 3 and its subpathname
             (merge-pathnames (make-pathname :defaults parent
                                             :directory (cons :relative directory)
                                             :name name :type type :version version)
                              parent))
           (here-directory ()
             (subpath (or *compile-file-truename* *load-truename*
                          (truename *default-pathname-defaults*))))
           (load-and-configure-asdf ()
             ;; First, try to require ASDF from the implementation, if not already loaded.
             ;; Most implementations provide ASDF 3.0, LispWorks still lags with ASDF 2.019,
             ;; and some unmaintained implementations, or obsolete implementations or versions thereof
             ;; only provide ASDF 2, ASDF 1, or don't provide ASDF.
             ;; Note that CLISP is case-sensitive, so we need to specify a lowercase string,
             ;; and not the keyword :asdf or symbol 'asdf; old CLISP versions that don't provide ASDF
             ;; may error at compile-time if we call (require "asdf") directly.
             (ignore-errors (funcall 'require "asdf"))
             ;; If ASDF 2 isn't provided, load our ASDF from source.
             ;; ASDF 1 is not enough, because it won't heed our project's output-translations.
             ;; (Beside, no one serious provides ASDF 1 anymore.)
             (unless (member :asdf2 *features*)
               (let ((asdf-lisp (asdf-lisp)))
                   (if (probe-file asdf-lisp)
                       (load asdf-lisp)
                       (error "This Lisp implementation fails to provide ASDF 2 or later"))))
               ;; Configure ASDF
               (configure-asdf)
               (let ((provided-version (asdf-version)))
                 ;; Upgrade ASDF to what we configured it to be.
                 (asdf-call 'load-system :asdf)
                 ;; If the implementation-provided version was too old,
                 ;; we need to re-configure, because old configuration may have been moved away.
                 (unless (asdf-call 'version-satisfies provided-version "2.27")
                   (configure-asdf)))
               (unless (asdf-call 'version-satisfies (asdf-version) (required-asdf-version))
                 (error "This program needs ASDF ~A but could only find ASDF ~A"
                        (required-asdf-version) (asdf-version))))
           ;; User-configurable parts
           (required-asdf-version () "3.1.2") ;; In the end, we want at least ASDF 3.1.2
             (asdf-lisp ()
               ;; Here, define where your Lisp source code hierarchy stores its copy of ASDF.
               ;; In your project, that might be :directory '("libraries" "asdf" "build")
               ;; Or NIL, if you don't do use any fancy ASDF feature, and
               ;; trust your implementation to provide a recent enough copy.
               (subpath (here-directory) :directory '(:back "build") :name "asdf" :type "lisp"))
           (configure-asdf ()
             (let* ((source-directory
                      ;; Here, define the top of your Lisp source code hierarchy.
                      ;; If you can assume an implementation that has ASDF 2 or later
                      ;; (you should: all serious ones do), you might compute it based on
                      ;;   (asdf-call 'getenv "MY_PROJECT_ROOT") instead of (here-directory).
                      ;; If you can assume an implementation that has ASDF 3 or later
                      ;; (you probably can: most serious ones do), you might use instead
                      ;;   (asdf-call 'getenv-pathname "MY_PROJECT_ROOT"
                      ;;     :want-absolute t :ensure-directory t)
                      (subpath (here-directory) :directory '(:back)))
                    (source-registry
                      (or (asdf-call 'getenv "ASDF_DEVEL_SOURCE_REGISTRY")
                          `(:source-registry
                            (:tree ,source-directory)
                            ;; In a fully controlled build, you'd :ignore-inherited-configuration instead:
                            :inherit-configuration)))
                    (output-directory
                      ;; There again, you might want to use some getenv variant.
                      ;; Also, "fasls" might be redundant for your project.
                      (subpath source-directory :directory '("build" "fasls")))
                    (output-translations
                      `(:output-translations
                        ;; Segregate output by ABI.
                        ;; You could replace "asdf" below by the name of your project,
                        ;; or not need it at all if everything is under your source-directory.
                        (,source-directory (,output-directory :implementation "asdf"))
                        ;; In a fully controlled build, we shouldn't be using code outside
                        ;; our source-directory, but in case we do, we still want to control the output,
                        ;; and easily detect the fact by looking at this directory
                        (t (,output-directory :implementation "root"))
                        ;; The above should already cover all paths that we use;
                        ;; we don't want user configuration to interfere with the build.
                        :ignore-inherited-configuration)))
               ;; No more user-configurable parts below.
               (asdf-call 'initialize-source-registry source-registry)
               (asdf-call 'initialize-output-translations output-translations))))
    ;; Configure the printer
    (setf *print-readably* nil ; allegro 5.0 may bork without this
          *print-level* nil)
    ;; Hush the compiler and loader
    (setf *load-verbose* nil *load-print* nil
          *compile-verbose* nil *compile-print* nil)
    ;; Load and configure ASDF
    (load-and-configure-asdf)))

#|
;;; Here is the much simpler code when you can assume
;;; your implementation provides ASDF 3 (i.e. at least pre-release ASDF 2.27).
;;; No package madness.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "asdf"))

#-asdf3 (error "Your implementation fails to provide ASDF 3")

(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; User-configurable parts
  (let* ((required-asdf-version "3.1.2") ;; In the end, we want at least ASDF 3.1.2
         (here-directory
           (pathname-directory-pathname
            (or *compile-file-truename* *load-truename*
                (truename *default-pathname-defaults*))))
         (source-directory
           ;; Here, define the top of your source code hierarchy.
           ;; For your project, it could be something like
           ;;   (or (getenv-pathname "MY_PROJECT_ROOT"
           ;;        :want-absolute t :ensure-directory t)
           ;;       (subpathname here-directory "../../"))
           (subpathname here-directory "../"))
         (source-registry
           ;; Here, define your source registry.
           ;; For your project, it would be based solely on source-directory above,
           ;; and in a fully controlled build, you'd :ignore-inherited-configuration
                  (or (getenvp "ASDF_DEVEL_SOURCE_REGISTRY")
                      `(:source-registry
                        (:tree ,source-directory)
                        :inherit-configuration)))
         (output-directory
           ;; There again, you might want to use some getenvp variant.
           ;; Also, "fasls" might be redundant for your project.
           (subpathname source-directory "build/fasls/"))
         (output-translations
           `(:output-translations
             ;; Segregate output by ABI.
             ;; You could replace "asdf" below by the name of your project,
             ;; or not need it at all if everything is under your source-directory.
             (,source-directory (,output-directory :implementation "asdf"))
             ;; In a fully controlled build, we shouldn't be using code outside
             ;; our source-directory, but in case we do, we still want to control the output,
             ;; and easily detect the fact by looking at this directory
             (t (,output-directory :implementation "root"))
             ;; The above should already cover all paths that we use;
             ;; we don't want user configuration to interfere with the build.
             :ignore-inherited-configuration)))
         ;; No more user-configurable parts below.
    ;; Configure the printer
    (setf *print-readably* nil ; allegro 5.0 may bork without this
          *print-level* nil)
    ;; Hush the compiler and loader
    (setf *load-verbose* nil *load-print* nil
          *compile-verbose* nil *compile-print* nil)
    ;; Configure ASDF
    (initialize-source-registry source-registry)
    (initialize-output-translations output-translations)
    ;; Upgrade to the latest configured version
    (upgrade-asdf)
    ;; Check that we have a satisfactorily version
    (unless (version-satisfies (asdf-version) required-asdf-version)
      (error "Please install an ASDF ~A or later" required-asdf-version))))
|#
