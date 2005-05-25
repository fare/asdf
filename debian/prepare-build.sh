#!/bin/sh

[ -d debian ] || exit 1

[ $(pwd | sed 's/.*\/\([^\/]*\)/\1/') == "asdf-cvs-master" ]  || exit 3

set -e

cvs -q -z3  update -APd .

cvs2cl --stdout > changelog
VISUAL=nvi EDITOR=nvi dch --increment --preserve 
DIR=`pwd`

rm -rf ../asdf || true
find . -type f -depth | cpio --pass-through --make-directories --link --preserve-modification-time ../asdf
cd ../asdf

find . -name CVS -print0 | xargs -0 rm -rvf
find . -name .cvsignore -print0 | xargs -0 rm -v

