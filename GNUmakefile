system	 	:= "asdf"
#user 		:= $(shell basename `echo "$home"`)
user := "gking"
webhome_private := $(user)@common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"

sourceDirectory := $(shell pwd)

# website, tag, install

install: archive-copy

archive: FORCE
	bin/build-tarball.sh

archive-copy: archive
	bin/rsync-cp.sh tmp/asdf*.tar.gz $(webhome_private)/archives
	bin/link-tarball.sh $(clnet_home) $(user)
	bin/rsync-cp.sh tmp/asdf.lisp $(webhome_private)

website-copy: FORCE
	bin/rsync-cp.sh website/output/ $(webhome_private)
	bin/rsync-cp.sh tmp/asdf.lisp $(webhome_private)

clean_dirs = $(sourceDirectory)
clean_extensions = fasl dfsl cfsl fasl fas lib

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

FORCE: