system	 	:= "asdf"
#user 		:= $(shell basename `echo "$home"`)
user := "frideau"
webhome_private := $(user)@common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"

sourceDirectory := $(shell pwd)

lisps ?= allegro ccl clisp ecl sbcl
## not tested by me: abcl allegromodern cmucl lisworks
## FAIL: gclcvs
## maybe supported by asdf, not supported yet by our tests: cormancl mcl scl

lisp ?= sbcl

# website, tag, install

install: archive-copy

bump_revision: FORCE
	bin/bump-revision-and-tag.sh

archive: FORCE
	sbcl --userinit /dev/null --sysinit /dev/null --load bin/make-helper.lisp \
		--eval "(rewrite-license)" --eval "(quit)"
	bin/build-tarball.sh

archive-copy: archive
	git checkout release
	bin/rsync-cp.sh tmp/asdf*.tar.gz $(webhome_private)/archives
	bin/link-tarball.sh $(clnet_home) $(user)
	bin/rsync-cp.sh tmp/asdf.lisp $(webhome_private)
	git push cl.net release
	git push --tags cl.net release

website:
	make -C doc website

clean_dirs = $(sourceDirectory)
clean_extensions = fasl dfsl cfsl fasl fas lib dx32fsl lx64fsl lx32fsl o bak

clean: FORCE
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
	rm -rf tmp
	make -C doc clean

test: FORCE
	@cd test; make clean;./run-tests.sh ${lisp} ${test-glob}

test-all: FORCE
	@for lisp in ${lisps} ; do \
		make test lisp=$$lisp || exit 1 ; \
	done

FORCE:
