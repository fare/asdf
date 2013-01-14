;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.

(asdf/package:define-package :asdf/interface
  (:nicknames :asdf :asdf-utilities)
  (:recycle :asdf/interface :asdf)
  (:unintern
   #:*asdf-revision* #:around #:asdf-method-combination
   #:do-traverse #:do-dep #:do-one-dep #:visit-action #:component-visited-p
   #:split #:make-collector
   #:loaded-systems ; makes for annoying SLIME completion
   #:output-files-for-system-and-operation) ; obsolete ASDF-BINARY-LOCATION function
  (:use :common-lisp :asdf/driver :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/output-translations :asdf/source-registry
   :asdf/plan :asdf/operate :asdf/defsystem :asdf/bundle :asdf/concatenate-source
   :asdf/backward-internals :asdf/backward-interface)
  ;; TODO: automatically generate interface with reexport?
  (:export
   #:defsystem #:find-system #:locate-system #:coerce-name
   #:oos #:operate #:traverse #:perform-plan
   #:system-definition-pathname #:with-system-definitions
   #:search-for-system-definition #:find-component #:component-find-path
   #:compile-system #:load-system #:load-systems
   #:require-system #:test-system #:clear-system
   #:operation #:upward-operation #:downward-operation
   #:load-op #:prepare-op #:compile-op #:load-fasl-op
   #:prepare-source-op #:load-source-op #:test-op
   #:feature #:version #:version-satisfies #:upgrade-asdf
   #:implementation-identifier #:implementation-type #:hostname
   #:input-files #:output-files #:output-file #:perform
   #:operation-done-p #:explain #:component-sibling-dependencies
   #:run-program/ ; the recommended replacement for run-shell-command
   #:component-load-dependencies #:run-shell-command ; deprecated, do not use
   #:precompiled-system #:compiled-file
   #+ecl #:make-build #+mkcl #:bundle-system

   #:component #:parent-component #:child-component #:system #:module
   #:source-file #:c-source-file #:java-source-file
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:static-file #:doc-file #:html-file :text-file
   #:source-file-type

   #:component-children          ; component accessors
   #:component-children-by-name
   #:component-pathname
   #:component-relative-pathname
   #:component-name
   #:component-version
   #:component-parent
   #:component-property
   #:component-system
   #:component-encoding
   #:component-external-format

   #:component-depends-on ; backward-compatible name rather than action-depends-on
   #:module-components ; backward-compatibility
   #:operation-on-warnings #:operation-on-failure ; backward-compatibility

   #:system-description
   #:system-long-description
   #:system-author
   #:system-maintainer
   #:system-license
   #:system-licence
   #:system-source-file
   #:system-source-directory
   #:system-relative-pathname
   #:map-systems

   #:operation-description

   #:*system-definition-search-functions*   ; variables
   #:*central-registry*
   #:*compile-file-warnings-behaviour*
   #:*compile-file-failure-behaviour*
   #:*resolve-symlinks*
   #:*load-system-operation*
   #:*asdf-verbose*
   #:*verbose-out*

   #:asdf-version

   #:operation-error #:compile-failed #:compile-warned #:compile-error
   #:error-name
   #:error-pathname
   #:load-system-definition-error
   #:error-component #:error-operation
   #:system-definition-error
   #:missing-component
   #:missing-component-of-version
   #:missing-dependency
   #:missing-dependency-of-version
   #:circular-dependency        ; errors
   #:duplicate-names

   #:try-recompiling
   #:retry
   #:accept                     ; restarts
   #:coerce-entry-to-directory
   #:remove-entry-from-registry

   #:*encoding-detection-hook*
   #:*encoding-external-format-hook*
   #:*default-encoding*
   #:*utf-8-external-format*

   #:clear-configuration
   #:*output-translations-parameter*
   #:initialize-output-translations
   #:disable-output-translations
   #:clear-output-translations
   #:ensure-output-translations
   #:apply-output-translations
   #:compile-file*
   #:compile-file-pathname*
   #:enable-asdf-binary-locations-compatibility
   #:*default-source-registries*
   #:*source-registry-parameter*
   #:initialize-source-registry
   #:compute-source-registry
   #:clear-source-registry
   #:ensure-source-registry
   #:process-source-registry
   #:system-registered-p #:registered-systems #:already-loaded-systems
   #:resolve-location
   #:asdf-message
   #:user-output-translations-pathname
   #:system-output-translations-pathname
   #:user-output-translations-directory-pathname
   #:system-output-translations-directory-pathname
   #:user-source-registry
   #:system-source-registry
   #:user-source-registry-directory
   #:system-source-registry-directory))
(in-package :asdf/interface)

(when-upgrade (:when (fboundp 'make-sub-operation))
  (defun* make-sub-operation (c o dep-c dep-o)
    (declare (ignore c o dep-c dep-o)) (asdf-upgrade-error)))
