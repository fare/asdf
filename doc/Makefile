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

log_output = > $@.log 2>&1 || { echo "  failed. See log in $@.log" ; exit 1 ;}

manual-html: asdf.texinfo
	@echo "makeinfo --html asdf.texinfo"
	@makeinfo --html asdf.texinfo \
        $(log_output)

asdf.html: asdf.texinfo
	@echo "makeinfo --html --no-split --no-headers -o asdf.html asdf.texinfo"
	@makeinfo --html --no-split --no-headers -o asdf.html asdf.texinfo \
        $(log_output)

asdf.info: asdf.texinfo
	@echo "makeinfo asdf.texinfo"
	@makeinfo asdf.texinfo \
        $(log_output)

asdf.pdf: asdf.texinfo
	@echo "texi2pdf asdf.texinfo"
	@texi2pdf asdf.texinfo \
        $(log_output)

website: all
	rsync -lt --no-g ${webfiles} ${website}
	rsync --recursive --no-g --delete asdf ${website}
	rsync -lt --no-g Changelog ${website}/changelog

.PHONY: clean intermediate-clean manual-html
intermediate-clean:
	rm -f ${intermediate}
clean: intermediate-clean
	rm -f asdf.html asdf.pdf asdf.info
	rm -rf asdf/ asdf.t2d/
