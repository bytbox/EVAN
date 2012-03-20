#!/bin/bash

# build.bash
# Builds all libraries and binaries.

set -e
set -o pipefail

cpipe() {
	sed -u "s/^/  > /"
}

EVANROOT=`pwd`

echo Creating directories...
rm -rf bin
mkdir -p bin lib analysis

EVANBIN=$EVANROOT/bin
HSINST="cabal install --bindir=$EVANROOT/bin"

echo Unpacking contrib...
tar xzf contrib.tgz

echo Checking python dependencies...
python3 -c 'import tkinter'

#echo Updating haskell package database...
#cabal update 2>&1 | cpipe

echo Installing EVAN haskell libraries...
cd $EVANROOT/evanlib
$HSINST 2>&1 | cpipe

echo Building mkref...
cd $EVANROOT/tools/mkref
$HSINST 2>&1 | cpipe

cd $EVANROOT

echo Generating documentation...
mkdir -p docs
perl -Icontrib/perl-json tools/xdocs.pl evanlib/EVAN > docs/reference.json
$EVANBIN/evan-mkref-html < docs/reference.json > docs/reference.html

echo Preparing python GUI...
cd $EVANROOT/gui
./genTools.pl
./genDocs.pl

if which go > /dev/null; then
	echo Building httpd...
	cd $EVANROOT/httpd
	go build
else
	echo "Go installation not found; not building httpd"
fi

cd $EVANROOT

echo Populating bin/...
ln -s `pwd`/tools/json2hs/json2hs.py bin/json2hs
ln -s `pwd`/scripts/evan-compile bin/evan-compile
tools/merge.pl gui/evan.py > bin/evan
chmod +x bin/evan
