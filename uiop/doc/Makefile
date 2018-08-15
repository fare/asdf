uiop := uiop
ifeq (${user},)
userat :=
else
userat := ${user}@
endif
website:=${userat}common-lisp.net:/project/asdf/public_html/
output = ${uiop}.html ${uiop}.pdf
webfiles = ${output}

.PHONY: clean html pdf include clean-include clean-crap info doc texinfo website

website: all
	rsync -lt --no-g ${webfiles} ${website}

all: pdf html info clean-crap

clean-include:
	rm -rf include

clean-crap:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr

clean: clean-include
	rm -f  *.pdf *.html *.info ${uiop}.texinfo

include:
	sbcl --no-userinit --eval '(require :asdf)' \
	--load docstrings.lisp \
	--eval '(sb-texinfo:generate-includes "include/" (list :UIOP/PACKAGE :UIOP/COMMON-LISP :UIOP/UTILITY :UIOP/VERSION :UIOP/OS :UIOP/PATHNAME :UIOP/FILESYSTEM :UIOP/STREAM :UIOP/IMAGE :UIOP/LISP-BUILD :UIOP/LAUNCH-PROGRAM :UIOP/RUN-PROGRAM :UIOP/CONFIGURATION :UIOP/BACKWARD-DRIVER))' \
	--eval '(quit)'

texinfo : include
	sbcl --no-userinit --eval '(require :asdf)' \
	--load docstrings.lisp \
	--eval "(sb-texinfo:complete-texinfo-template \"template.texinfo\" \"${uiop}.texinfo\" \"include/\")" \
	--eval '(quit)'

pdf: texinfo
	texi2pdf ${uiop}.texinfo

html: texinfo
	makeinfo --html --no-split ${uiop}.texinfo

info: texinfo
	makeinfo ${uiop}.texinfo
