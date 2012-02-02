#!/bin/sh

# Script to construct a binary distribution of EVAN for the current
# architecture and operating system. We do not assume that build.sh has been
# run.

set -e

./build.sh

ARCH=`uname -m`
OS=`uname -s`
VER=`git tag | tail -n1`
INCS="bin docs examples lib install.sh LICENSE README"

DNAME="EVAN-$VER-$OS-$ARCH"

mkdir -p $DNAME
cp -r $INCS $DNAME

echo Preparing tarball...
tar czf $DNAME.tgz $DNAME

rm -rf $DNAME
