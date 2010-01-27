#!/bin/sh

# make sure that there are no pending comments

# make sure you're on master
branch=` git branch | grep "\* "`
if [ "$branch" != "* master" ]; then
  echo "error: git branch is $branch, not master"
  exit -1
fi 

tag=`git describe --tag`
if [ "$tag" == "" ]; then
  echo "error: git tag not found"
  exit -1
fi

major=`expr //$tag : '//\(.*\)\.'`
minor=`expr //$tag : '.*\.\(.*\)'`
minor=`echo "$minor" | gawk -F - '{print $1}'`
if [ "$major" == "" ]; then
  echo "error: unable to parse major version in $tag"
  exit -1
fi
if [ "$minor" == "" ]; then
  echo "error: unable to parse minor version in $tag"
  exit -1
fi

bumped=`expr $minor + 1`
if [ ! "$?" == "0" ]; then
    echo "Unable to compute new version from $minor"
    exit -2
fi
new_version="$major.$bumped"

cp asdf.lisp asdf.bak
perl -pi -e "s/VERSION:[^\"]*\"/VERSION:$new_version\"/" asdf.lisp
if [ ! "$?" == "0" ]; then
    echo "Unable to perl replace version"
    exit -3
fi

echo "Update reversion and commit"
git add asdf.lisp
if [ ! "$?" == "0" ]; then
    echo "Unable to git add"
    exit -4
fi
git commit -m "update ASDF version to $new_version"
if [ ! "$?" == "0" ]; then
    echo "Unable to git commit"
    exit -5
fi

bin/tag-release.sh $new_version
