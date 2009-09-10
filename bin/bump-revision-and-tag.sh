#!/bin/sh

# make sure that there are no pending comments

# make sure you're on master
branch=` git branch | grep "\* "`
if [ "$branch" != "* master" ]; then
  echo "error: git branch is $branch, not master"
  exit -1
fi 
exit 0

tag=`git describe --tag`
if [ "$tag" == "" ]; then
  echo "error: git tag not found"
  exit -1
fi

major=`expr //$tag : '//\(.*\)\.'`
minor=`expr //$tag : '.*\.\(.*\)'`

if [ "$major" == "" ]; then
  echo "error: unable to parse major version in $tag"
  exit -1
fi
if [ "$minor" == "" ]; then
  echo "error: unable to parse minor version in $tag"
  exit -1
fi

bumped=`expr $minor + 1`
new_version="$major.$bumped"

cp asdf.lisp asdf.bak
perl -pi -e "s/REVISION:.*\"/REVISION:$new_version\"/" asdf.lisp

echo "Update reversion and commit"
git add asdf.lisp
git commit -m "update ASDF version to $new_version"

bin/tag-release.sh $new_version
