UIOP := uiop

.PHONY: clean html pdf include clean-include clean-crap info doc

doc: pdf html info clean-crap

clean-include:
	rm -rf include

clean-crap:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr

clean: clean-include
	rm -f  *.pdf *.html *.info

include:
	sbcl --no-userinit --eval '(require :asdf)' \
	--load docstrings.lisp \
	--eval '(sb-texinfo:generate-includes "include/" (list :UIOP/PACKAGE :UIOP/COMMON-LISP :UIOP/UTILITY :UIOP/VERSION :UIOP/OS :UIOP/PATHNAME :UIOP/FILESYSTEM :UIOP/STREAM :UIOP/IMAGE :UIOP/LISP-BUILD :UIOP/LAUNCH-PROGRAM :UIOP/RUN-PROGRAM :UIOP/CONFIGURATION :UIOP/BACKWARD-DRIVER))' \
	--eval "(sb-texinfo:complete-texinfo-template \"template.texinfo\" \"$(UIOP).texinfo\" \"include/\")" \
	--eval '(quit)'

pdf: include
	texi2pdf $(UIOP).texinfo

html: include
	makeinfo --html --no-split $(UIOP).texinfo

info: include
	makeinfo $(UIOP).texinfo
