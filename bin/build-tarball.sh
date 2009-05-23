#!/bin/sh

# create tarball and tagged asdf.lisp file in tmp/ using the most recent
# annotated tag


tag=`git describe --tags`
if [ "$tag" == "" ]; then
    echo "Unable to find most recent tag, exiting"
    exit 1
fi

if [ -d "tmp" ]; then
    rm -r tmp
fi
mkdir tmp

archive_file="tmp/asdf-$tag.tar.gz"

echo "Create tmp/asdf.tar.gz with tag $tag"
git archive $tag --prefix="asdf/" --format=tar | \
    gzip > $archive_file
echo "Extract tmp/asdf.lisp"
tar --to-stdout -zxf $archive_file asdf/asdf.lisp > tmp/asdf.lisp

