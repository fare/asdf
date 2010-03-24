manual-html: asdf.texinfo
	makeinfo --html asdf.texinfo

asdf.html: asdf.texinfo
	makeinfo --html --no-split --no-headers -o asdf.html asdf.texinfo

asdf.info: asdf.texinfo
	makeinfo asdf.texinfo

asdf.pdf: asdf.texinfo
	texi2pdf asdf.texinfo

