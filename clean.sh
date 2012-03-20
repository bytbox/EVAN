#!/bin/sh

# clean.sh
# Removes all files generated by build.sh.

set -e

EVANROOT=`pwd`

rm -rf bin lib contrib docs gui/__pycache__ gui/fdocs.py gui/ftools.py

HSCLEAN="evanlib tools/mkref"

for d in $HSCLEAN; do
	echo Cleaning $d...
	cd $EVANROOT/$d
	cabal clean -v0
done

if which go > /dev/null; then
	echo Cleaning httpd...
	cd $EVANROOT/httpd
	go clean
fi

