#!/bin/sh

set -e

EVANROOT=`pwd`

rm -rf analysis bin docs gui/__pycache__

HSCLEAN="lib www tools/mkref"

for d in $HSCLEAN; do
	cd $EVANROOT/$d
done

