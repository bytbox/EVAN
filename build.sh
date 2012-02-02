#!/bin/sh

# build.sh
# Builds all libraries and binaries.

set -e

EVANROOT=`pwd`

echo Checking python dependencies...
python3 -c 'import matplotlib; import numpy; import tkinter'

echo Checking haskell dependencies...
echo "  Installing json with mapdict flagged..."
cabal install -v2 json -f mapdict | sed -u "s/^/  > /"

echo Installing EVAN haskell libraries...
pushd . > /dev/null
cd lib
cabal install 2>&1 | sed -u "s/^/  > /"
popd > /dev/null

echo Populating bin/...
mkdir -p bin
rm -f bin/*
ln -s `pwd`/tools/json2hs/json2hs.py bin/json2hs
ln -s `pwd`/scripts/evan-compile bin/evan-compile
tools/merge.pl gui/evan.py > bin/evan
chmod +x bin/evan

echo Building mkref...
pushd . > /dev/null
cd tools/mkref
cabal install --bindir=$EVANROOT/bin 2>&1 | sed -u "s/^/  > /"
popd > /dev/null

echo Generating documentation...
mkdir -p docs
tools/xdocs.pl lib/EVAN > docs/reference.json

echo Preparing python GUI...
pushd . > /dev/null
cd gui
./genTools.pl
./genDocs.pl
popd > /dev/null

echo Building httpd...
pushd . > /dev/null
cd www
popd > /dev/null

mkdir -p analysis
