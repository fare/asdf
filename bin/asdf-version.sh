#!/bin/sh

# write the highest tag to standard output
# exit code is 1 if it cannot be found


tag=`git describe --tags`
if [ "$tag" == "" ]; then
    exit 1
fi
echo $tag
exit 0
