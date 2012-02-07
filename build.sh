#!/bin/sh

# build.sh
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
python3 -c 'import matplotlib; import numpy; import tkinter'

#echo Updating haskell package database...
#cabal update 2>&1 | cpipe

echo Checking haskell dependencies...
$HSINST json 2>&1 | cpipe

echo Installing EVAN haskell libraries...
pushd . > /dev/null
cd evanlib
$HSINST 2>&1 | cpipe
popd > /dev/null

echo Building mkref...
pushd . > /dev/null
cd tools/mkref
$HSINST 2>&1 | cpipe
popd > /dev/null

echo Generating documentation...
mkdir -p docs
perl -Icontrib/perl-json tools/xdocs.pl evanlib/EVAN > docs/reference.json
$EVANBIN/evan-mkref-html < docs/reference.json > docs/reference.html

echo Preparing python GUI...
pushd . > /dev/null
cd gui
./genTools.pl
./genDocs.pl
popd > /dev/null

echo Building httpd...
pushd . > /dev/null
cd httpd
popd > /dev/null

echo Populating bin/...
ln -s `pwd`/tools/json2hs/json2hs.py bin/json2hs
ln -s `pwd`/scripts/evan-compile bin/evan-compile
tools/merge.pl gui/evan.py > bin/evan
chmod +x bin/evan
