system	 	:= "asdf"
webhome_private := common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"
sourceDirectory := $(shell pwd)

lisps ?= ccl clisp sbcl ecl abcl xcl scl allegro
## occasionally tested by not me: allegromodern cmucl lispworks
## FAIL: gclcvs (condition handling)
## tentatively supported by asdf, not supported by our tests: cormancl mcl genera

lisp ?= sbcl

# website, tag, install

install: archive-copy

archive:
	sbcl --userinit /dev/null --sysinit /dev/null --load bin/make-helper.lisp \
		--eval "(rewrite-license)" --eval "(quit)"
	bin/make-tarball

archive-copy: archive
	git checkout release
	bin/rsync-cp tmp/asdf*.tar.gz $(webhome_private)/archives
	bin/link-tarball $(clnet_home)
	bin/rsync-cp tmp/asdf.lisp $(webhome_private)
	${MAKE} push

push:
	git status
	git push --tags cl.net release master
	git push --tags xcvb release master
	git fetch
	git status

website:
	make -C doc website

clean_dirs = $(sourceDirectory)
clean_extensions = fasl dfsl cfsl fasl fas lib dx32fsl lx64fsl lx32fsl o bak x86f

clean:
	@for dir in $(clean_dirs); do \
	     if test -d $$dir; then \
	     	 echo Cleaning $$dir; \
		 for ext in $(clean_extensions); do \
		     find $$dir \( -name "*.$$ext" \) \
	     	    -and -not -path \""*/.git/*"\" \
		     	  -and -not -path \""*/_darcs/*"\" \
	     		  -and -not -path \""*/tags/*"\" -print -delete; \
		done; \
	     fi; \
	done
	rm -rf tmp/ LICENSE test/try-reloading-dependency.asd
	make -C doc clean

mrproper: clean
	rm -rf .pc/ build-stamp debian/patches/ debian/debhelper.log debian/cl-asdf/ # debian crap

test-upgrade:
	if [ -f /usr/lib/sbcl/sbcl-dist.core ] ; then \
		SBCL="/usr/bin/sbcl --core /usr/lib/sbcl/sbcl-dist.core" ; fi ; \
	mkdir -p tmp/ ; \
	for tag in 1.37 1.97 1.369 `git tag -l '2.0??'` ; do \
	  echo "Testing upgrade from ASDF $${tag}" ; \
	  git show $${tag}:asdf.lisp > tmp/asdf-$${tag}.lisp ; \
	  $${SBCL:-sbcl} --noinform --eval \
	'(progn (handler-bind ((t #'"'"'muffle-warning)) (load "tmp/asdf-'$${tag}'.lisp")) (handler-bind ((sb-kernel:redefinition-warning #'"'"'muffle-warning)((or warning error) (lambda (c) (format t "~A~%" c) #|(defparameter *c* c) (break)|# (sb-ext:quit :unix-status 1)))) (load "asdf.lisp") (format t "Successfully upgraded from '$${tag}'~%") (sb-ext:quit :unix-status 0)))' || \
	{ echo "FAILED" ; exit 1 ; } ; done

test-forward-references:
	if [ -f /usr/lib/sbcl/sbcl-dist.core ] ; then SBCL="/usr/bin/sbcl --core /usr/lib/sbcl/sbcl-dist.core" ; fi ; $${SBCL:-sbcl} --noinform --load ~/cl/asdf/asdf.lisp --eval '(sb-ext:quit)' 2>&1 | cmp - /dev/null

do-test:
	@cd test; make clean;./run-tests.sh ${lisp} ${test-glob}

test: do-test test-forward-references

do-test-all:
	@for lisp in ${lisps} ; do \
		make do-test lisp=$$lisp || exit 1 ; \
	done

test-all: test-forward-references test-upgrade do-test-all

# Note that the debian git at git://git.debian.org/git/pkg-common-lisp/cl-asdf.git is stale,
# as we currently build directly from upstream at git://common-lisp.net/projects/asdf/asdf.git
debian-package: mrproper
	: $${RELEASE:="$$(git tag -l '2.0[0-9][0-9]' | tail -n 1)"} ; \
	git-buildpackage --git-debian-branch=release --git-upstream-branch=$$RELEASE --git-tag --git-retag --git-ignore-branch

# Replace SBCL's ASDF with the current one.
# for casual users, just use (asdf:load-system :asdf)
replace-sbcl-asdf:
	sbcl --eval '(compile-file "asdf.lisp" :output-file (format nil "~Aasdf/asdf.fasl" (sb-int:sbcl-homedir-pathname)))' --eval '(quit)'

# Replace CCL's ASDF with the current one.
# for casual users, just use (asdf:load-system :asdf)
replace-ccl-asdf:
	ccl --eval '(progn(compile-file "asdf.lisp" :output-file (format nil "~Atools/asdf.lx64fsl" (ccl::ccl-directory)))(quit))'

WRONGFUL_TAGS := 1.1720 README RELEASE STABLE
# Delete wrongful tags from local repository
fix-local-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git tag -d $$i ; done

# Delete wrongful tags from remote repository
fix-remote-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git push $${REMOTE:-cl.net} :refs/tags/$$i ; done

TODO:
	exit 2

release: TODO test-all test-on-other-machines-too debian-changelog debian-package send-mail-to-mailing-lists

.PHONY: install archive archive-copy push website clean mrproper \
	upgrade-test test-forward-references test do-test test-all do-test-all \
	debian-package release \
	replace-sbcl-asdf replace-ccl-asdf \
	fix-local-git-tags fix-remote-git-tags
