#!/bin/sh

# clean.sh
# Removes all files generated by build.sh.

set -e

EVANROOT=`pwd`

rm -rf bin docs gui/__pycache__

HSCLEAN="evanlib www tools/mkref"

for d in $HSCLEAN; do
	cd $EVANROOT/$d
	cabal clean -v0
done

