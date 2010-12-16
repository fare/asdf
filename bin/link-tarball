#!/bin/sh

if [ -z $1 ]; then
  echo "Remote directory must be specified."
  exit 1
fi
if [ -z $2 ]; then
  echo "Remote username must be specified."
  exit 1
fi
clnet_home="$1"
user=$2

tarball=`ls tmp/asdf*.tar.gz`
tarball=`basename $tarball`
latest="asdf.tar.gz"

echo "Link $tarball to $latest"
ssh $user@common-lisp.net "rm -f $clnet_home$latest; \
  ln -s $clnet_home/archives/$tarball $clnet_home$latest"
