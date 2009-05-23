#!/bin/sh

# tag the currently checked out branch with $tag and update
# the tag RELEASE to point at this. $tag must not already exist.

# create tarball and tagged asdf.lisp file in tmp/


if [ -z $1 ]; then
  echo "Tag must be specified."
  exit 1
fi

tag=$1
system="asdf"
git tag | grep -q $tag > /dev/null
if [ $? -eq 0 ]; then
    echo "Tag $tag already exists. Exiting."
    exit 1
fi

echo "Tagging with $tag"
git tag $tag
echo "Tagging with RELEASE"
git tag -f RELEASE

bin/build-tarball.sh
