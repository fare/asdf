#!/bin/sh

#        --extended-attributes \

rsync \
 	--archive \
	--rsh=ssh \
	--compress \
	--partial \
	--include "*/" --include "*" \
        --progress \
	-v \
	$*
