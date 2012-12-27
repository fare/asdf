system	 	:= "asdf"
webhome_private := common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"
sourceDirectory := $(shell pwd)

ifdef ASDF_TEST_LISPS
lisps ?= ${ASDF_TEST_LISPS}
else
lisps ?= ccl clisp sbcl ecl ecl_bytecodes cmucl abcl scl allegro lispworks allegromodern xcl
endif

## MAJOR FAIL: gclcvs -- COMPILER BUG! Upstream fixed it, but upstream fails to compile.
## NOT SUPPORTED BY OUR TESTS: cormancl genera lispworks-personal-edition mkcl rmcl. Manually tested once in a while.

lisp ?= sbcl

CCL ?= ccl
CLISP ?= clisp
SBCL ?= sbcl
ECL ?= ecl
MKCL ?= mkcl
CMUCL ?= cmucl
ABCL ?= abcl
XCL ?= xcl
SCL ?= scl
ALLEGRO ?= alisp
ALLEGROMODERN ?= mlisp
LISPWORKS ?= lispworks

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
	# 1.37 is the last release by Daniel Barlow
	# 1.97 is the last release before Gary King takes over
	# 1.369 is the last release by Gary King
	# 2.000 to 2.019 and 2.20 to 2.27 and beyond are Faré's "stable" releases
	fasl=fasl ; \
	use_ccl () { li="${CCL} --no-init --quiet" ; ev="--eval" ; } ; \
	use_clisp () { li="${CLISP} -norc -ansi --quiet --quiet" ; ev="-x" ; } ; \
	use_sbcl () { li="${SBCL} --noinform --no-userinit" ; ev="--eval" ; } ; \
	use_ecl () { li="${ECL} -norc" ; ev="-eval" ; } ; \
	use_ecl_bytecodes () { li="${ECL} -norc -eval (ext::install-bytecodes-compiler)" ; ev="-eval" ; } ; \
	use_mkcl () { li="${MKCL} -norc" ; ev="-eval" ; } ; \
	use_cmucl () { li="${CMUCL} -noinit" ; ev="-eval" ; } ; \
	use_abcl () { li="${ABCL} --noinit --nosystem --noinform" ; ev="--eval" ; } ; \
	use_xcl () { li="${XCL} --noinit --nosystem --noinform" ; ev="--eval" ; } ; \
	use_scl () { li="${SCL} -noinit" ; ev="-eval" ; } ; \
	use_allegro () { li="${ALLEGRO} -q" ; ev="-e" ; } ; \
	use_allegromodern () { li="${ALLEGROMODERN} -q" ; ev="-e" ; } ; \
	use_lispworks () { li="${LISPWORKS} -siteinit - -init -" ; ev="-eval" ; } ; \
	use_${lisp} ; \
	su=test/script-support.lisp ; lu="(load\"$$su\")" ; \
	lv="$$li $$ev $$lu $$ev" ; \
	for tag in 1.37 1.97 1.369 `git tag -l '2.0??'` `git tag -l '2.??'` ; do \
	  rm -f $$fa ; \
	  for x in load-system load-lisp load-lisp-compile-load-fasl load-fasl just-load-fasl ; do \
	    lo="(asdf-test::load-old-asdf \"$${tag}\")" ; \
	    echo "Testing upgrade from ASDF $${tag} using method $$x" ; \
	    git show $${tag}:asdf.lisp > tmp/asdf-$${tag}.lisp ; \
	    case ${lisp}:$$tag:$$x in \
	      abcl:2.0[01][1-9]|abcl:2.2[1-2]:*) \
		: Skip, because it is so damn slow ;; \
	      ccl:1.*|ccl:2.0[01]*) \
		: Skip, because ccl broke old asdf ;; \
	      cmucl:1.*|cmucl:2.00*|cmucl:2.01[0-4]:*) \
		: Skip, CMUCL has problems before 2.014.7 due to source-registry upgrade ;; \
	      ecl*:1.*|ecl*:2.0[01]*|ecl*:2.20:*) \
		: Skip, because of various ASDF issues ;; \
	      mkcl:1.*|mkcl:2.0[01]*|mkcl:2.2[0-3]:*) \
		: Skip, because MKCL is only supported starting with 2.24 ;; \
	      xcl:1.*|xcl:2.00*|xcl:2.01[0-4]:*) \
		: XCL support starts with ASDF 2.014.2 ;; \
	      *) (set -x ; \
                  case $$x in \
		    load-system) l="$$lo (asdf-test::load-asdf-system)" ;; \
		    load-lisp) l="$$lo (asdf-test::load-asdf-lisp)" ;; \
		    load-lisp-compile-load-fasl) l="$$lo (asdf-test::compile-load-asdf)" ;; \
		    load-fasl) l="$$lo (asdf-test::load-asdf-fasl)" ;; \
		    just-load-fasl) l="(asdf-test::load-asdf-fasl)" ;; \
		    *) echo "WTF?" ; exit 2 ;; esac ; \
		  $$lv "(asdf-test::test-asdf $$l)" ) || \
		{ echo "upgrade FAILED" ; exit 1 ;} ;; esac ; \
	done ; done 2>&1 | tee tmp/results/${lisp}.upgrade.text

test-forward-references:
	${SBCL} --noinform --no-userinit --no-sysinit --load asdf.lisp --load test/script-support.lisp --eval '(asdf-test::exit-lisp 0)' 2>&1 | cmp - /dev/null

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

test-all-upgrade:
	@for lisp in ${lisps} ; do \
		${MAKE} test-upgrade lisp=$$lisp || exit 1 ; \
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

# RELEASE checklist:
# make test-all
# ./bin/bump-version 2.27
# edit debian/changelog
# make release-push archive-copy website debian-package
# dput mentors ../*.changes
# send debian mentors request
# send announcement to asdf-announce, asdf-devel, etc.
