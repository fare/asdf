#user=frideau
ifeq (${user},)
userat :=
else
userat := ${user}@
endif
website:=${userat}common-lisp.net:/project/asdf/public_html/
output = asdf.html asdf asdf.pdf asdf.info
webfiles = index.html .htaccess style.css cclan.png lisp-logo120x80.png favicon.ico ${output}
intermediate = asdf.cps asdf.log asdf.vr asdf.aux asdf.fn asdf.toc asdf.vrs \
		asdf.cp asdf.fns asdf.ky asdf.pg asdf.tp asdf.tps

all:	asdf.html asdf.info asdf.pdf manual-html

version.texinfo: ../version.lisp-expr
	head -n 1 $^ | perl -ne 'm|\"(.*)\"|; print "\@set VERSION $$1\n"' > $@

manual-html: asdf.texinfo version.texinfo
	makeinfo --html asdf.texinfo \
	2>&1 > manual-html.log || { echo "failed. See log in manual-html.log" ; exit 1 ;}

asdf.html: asdf.texinfo version.texinfo
	makeinfo --html --no-split --no-headers -o asdf.html asdf.texinfo \
	2>&1 > asdf.html.log || { echo "failed. See log in asdf.html.log" ; exit 1 ;}

asdf.info: asdf.texinfo version.texinfo
	makeinfo asdf.texinfo

asdf.pdf: asdf.texinfo version.texinfo
	texi2pdf asdf.texinfo \
	2>&1 > asdf.pdf.log || { echo "failed. See log in asdf.pdf.log" ; exit 1 ;}

website: all
	rsync -lt --no-g ${webfiles} ${website}
	rsync -lt --no-g ../debian/changelog ${website}/changelog

.PHONY: clean veryclean manual-html
clean:
	rm -f ${intermediate}
veryclean: clean
	rm -f asdf.html asdf.pdf asdf.info
	rm -rf asdf/ asdf.t2d/
