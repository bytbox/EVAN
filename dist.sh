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

tar czf "evan-$VER-$OS-$ARCH.tar.gz" $INCS

