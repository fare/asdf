system	 	:= "asdf"
webhome_private := common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"
sourceDirectory := $(shell pwd)

ifdef ASDF_TEST_LISPS
lisps ?= ${ASDF_TEST_LISPS}
else
lisps ?= ccl clisp sbcl ecl cmucl abcl scl allegro lispworks
endif

## MINOR FAIL: ecl-bytecodes (failure in test-compile-file-failure.script)
## MINOR FAIL: xcl (logical pathname issue in asdf-pathname-test.script)
## OCCASIONALLY TESTED BY NOT ME: allegromodern (not in my free demo version)
## MAJOR FAIL: gclcvs -- COMPILER BUG! Upstream fixed it, but upstream fails to compile.
## NOT SUPPORTED BY OUR TESTS: cormancl genera rmcl. Manually tested once in a while.

lisp ?= sbcl

CCL ?= ccl
CLISP ?= clisp
SBCL ?= sbcl
ECL ?= ecl
CMUCL ?= cmucl
ABCL ?= abcl
SCL ?= scl
ALLEGRO ?= alisp
ALLEGROMODERN ?= mlisp

# website, tag, install

default: test

install: archive-copy

archive:
	${SBCL} --userinit /dev/null --sysinit /dev/null --load bin/make-helper.lisp \
		--eval "(rewrite-license)" --eval "(quit)"
	bin/make-tarball

archive-copy: archive
	git checkout release
	bin/rsync-cp tmp/asdf*.tar.gz $(webhome_private)/archives
	bin/link-tarball $(clnet_home)
	bin/rsync-cp tmp/asdf.lisp $(webhome_private)
	${MAKE} push
	git checkout master

push:
	git status
	git push --tags cl.net release master
	git fetch
	git status

doc:
	${MAKE} -C doc

website:
	${MAKE} -C doc website

clean_dirs = $(sourceDirectory)
clean_extensions = fasl dfsl cfsl fasl fas lib dx32fsl lx64fsl lx32fsl ufasl o bak x86f vbin amd64f sparcf sparc64f hpf hp64f

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
	${MAKE} -C doc clean

mrproper: clean
	rm -rf .pc/ build-stamp debian/patches/ debian/debhelper.log debian/cl-asdf/ # debian crap

test-upgrade:
	mkdir -p tmp/fasls/${lisp} ; \
	fa=tmp/fasls/sbcl/upasdf.fasl ; \
	ll="(handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning)) (format t \"ll~%\") (load \"asdf.lisp\"))" ; \
	cf="(handler-bind ((warning #'muffle-warning)) (format t \"cf~%\") (compile-file \"asdf.lisp\" :output-file \"$$fa\" :verbose t :print t))" ; \
	lf="(handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning)) (format t \"lf\") (load \"$$fa\" :verbose t :print t))" ; \
	te="(asdf-test::quit-on-error $$l (push #p\"${sourceDirectory}/test/\" asdf:*central-registry*) (princ \"te\") (asdf:oos 'asdf:load-op :test-module-depend :verbose t))" ; \
	use_ccl () { li="${CCL} --no-init --quiet --load" ; ev="--eval" ; } ; \
	use_clisp () { li="${CLISP} -norc -ansi --quiet --quiet -i" ; ev="-x" ; } ; \
	use_sbcl () { li="${SBCL} --noinform --no-userinit --load" ; ev="--eval" ; } ; \
	use_ecl () { li="${ECL} -norc -load" ; ev="-eval" ; } ; \
	use_cmucl () { li="${CMUCL} -noinit -load" ; ev="-eval" ; } ; \
	use_abcl () { li="${ABCL} --noinit --nosystem --noinform --load" ; ev="--eval" ; } ; \
	use_scl () { li="${SCL} -noinit -load" ; ev="-eval" ; } ; \
	use_allegro () { li="${ALLEGRO} -q -L" ; ev="-e" ; } ; \
	use_allegromodern () { li="${ALLEGROMODERN} -q -L" ; ev="-e" ; } ; \
	su=test/script-support ; \
	for tag in 1.37 1.97 1.369 `git tag -l '2.0??'` `git tag -l '2.??'` ; do \
	  use_${lisp} ; \
	  lo="(handler-bind ((warning #'muffle-warning)) (load \"tmp/asdf-$${tag}.lisp\"))" ; \
	  echo "Testing upgrade from ASDF $${tag}" ; \
	  git show $${tag}:asdf.lisp > tmp/asdf-$${tag}.lisp ; \
	  rm -f $$fa ; lv="$$li $$su $$ev" ; \
	  case ${lisp}:$$tag in \
	    ecl:2.00[0-9]|ecl:2.01[0-6]) : Skip, because of get-uid ugliness ;; *) \
	  ( set -x ; $$lv "$$lo" $$ev "$$ll" $$ev "$$te" && \
	    $$lv "$$lo" $$ev "$$ll" $$ev "$$cf" $$ev "$$lf" $$ev "$$te" && \
	    $$lv "$$lo" $$ev "$$lf" $$ev "$$te" && \
	    $$lv "$$lf" $$ev "$$te" ) || { echo "upgrade FAILED" ; exit 1 ;} \
	  ;; esac ; \
	done

test-forward-references:
	${SBCL} --noinform --no-userinit --no-sysinit --load asdf.lisp --eval '(sb-ext:quit)' 2>&1 | cmp - /dev/null

test-lisp:
	@cd test; ${MAKE} clean;./run-tests.sh ${lisp} ${test-glob}

test: test-lisp test-forward-references doc

test-all-lisps:
	@for lisp in ${lisps} ; do \
		${MAKE} test-lisp test-upgrade lisp=$$lisp || exit 1 ; \
	done

# test upgrade is a very long run... This does just the regression tests
test-all-noupgrade:
	@for lisp in ${lisps} ; do \
		${MAKE} test-lisp lisp=$$lisp || exit 1 ; \
	done

test-all: test-forward-references doc test-all-lisps

# Note that the debian git at git://git.debian.org/git/pkg-common-lisp/cl-asdf.git is stale,
# as we currently build directly from upstream at git://common-lisp.net/projects/asdf/asdf.git
debian-package: mrproper
	: $${RELEASE:="$$(git tag -l '2.[0-9][0-9]' | tail -n 1)"} ; \
	git-buildpackage --git-debian-branch=release --git-upstream-branch=$$RELEASE --git-tag --git-retag --git-ignore-branch

# Replace SBCL's ASDF with the current one. -- Not recommended now that SBCL has ASDF2.
# for casual users, just use (asdf:load-system :asdf)
replace-sbcl-asdf:
	${SBCL} --eval '(compile-file "asdf.lisp" :output-file (format nil "~Aasdf/asdf.fasl" (sb-int:sbcl-homedir-pathname)))' --eval '(quit)'

# Replace CCL's ASDF with the current one. -- Not recommended now that CCL has ASDF2.
# for casual users, just use (asdf:load-system :asdf)
replace-ccl-asdf:
	${CCL} --eval '(progn(compile-file "asdf.lisp" :output-file (compile-file-pathname (format nil "~Atools/asdf.lisp" (ccl::ccl-directory))))(quit))'

WRONGFUL_TAGS := 1.37 1.1720 README RELEASE STABLE
# Delete wrongful tags from local repository
fix-local-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git tag -d $$i ; done

# Delete wrongful tags from remote repository
fix-remote-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git push $${REMOTE:-cl.net} :refs/tags/$$i ; done

release-push:
	git checkout master
	git merge release
	git checkout release
	git merge master
	git checkout master

TODO:
	exit 2

release: TODO test-all test-on-other-machines-too debian-changelog debian-package send-mail-to-mailing-lists

.PHONY: install archive archive-copy push doc website clean mrproper \
	upgrade-test test-forward-references test test-lisp test-upgrade test-forward-references \
	test-all test-all-lisps test-all-noupgrade \
	debian-package release \
	replace-sbcl-asdf replace-ccl-asdf \
	fix-local-git-tags fix-remote-git-tags
