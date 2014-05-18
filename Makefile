# -*- Makefile -*- This minimal Makefile delegates most work to the asdf-tools script
#
# End-Users, all you need to do is:
#   make
#
# Vendors, you may want to test your implementation with:
#   make test l=sbcl
#
# Other targets are for maintainer use only.
#

# Default action: bootstrap asdf.lisp
# That's the only thing that we really need before we may invoke asdf-builder.
all: build/asdf.lisp
	@: # This dummy action is necessary so the all target does not invoke the fallback action.

header_lisp := header.lisp
driver_lisp := uiop/package.lisp uiop/common-lisp.lisp uiop/utility.lisp uiop/os.lisp uiop/pathname.lisp uiop/filesystem.lisp uiop/stream.lisp uiop/image.lisp uiop/run-program.lisp uiop/lisp-build.lisp uiop/configuration.lisp uiop/backward-driver.lisp uiop/driver.lisp
defsystem_lisp := upgrade.lisp component.lisp system.lisp cache.lisp find-system.lisp find-component.lisp operation.lisp action.lisp lisp-action.lisp plan.lisp operate.lisp output-translations.lisp source-registry.lisp backward-internals.lisp parse-defsystem.lisp bundle.lisp concatenate-source.lisp backward-interface.lisp package-inferred-system.lisp interface.lisp user.lisp footer.lisp

# Making ASDF itself should be our first, default, target:
build/asdf.lisp: $(header_lisp) $(driver_lisp) $(defsystem_lisp)
	mkdir -p build
	rm -f $@
	cat $^ > $@

# These targets are used during tests to ensure the Makefile is in synch with the .asd files.
driver-files:
	@echo $(driver_lisp)

defsystem-files:
	@echo $(defsystem_lisp)

### exclude source files from fallback rule.
%.lisp:
	@:
Makefile:
	@:

### Default fall back rule: delegate to asdf-tools.
%: build/asdf.lisp
	@echo "Delegating $@ to asdf-tools" ; \
	./tools/asdf-tools make-target $@ l="$l" L="$L" u="$u" U="$u" v="$v" s="$s" t="$t"

.PHONY: all driver-files defsystem-files
