#!/bin/sh

set -e

echo Preparing python GUI...
python3 -c 'import matplotlib; import numpy; import tkinter'
pushd . > /dev/null
cd gui
./genTools.pl
./genDocs.pl
popd > /dev/null

echo Installing EVAN haskell libraries...
pushd . > /dev/null
cd lib
cabal install | sed -u "s/^/  /"
popd > /dev/null

echo Populating bin/...
mkdir -p bin
rm -f bin/*
ln -s `pwd`/tools/json2hs/json2hs.py bin/json2hs
ln -s `pwd`/scripts/evan-compile bin/evan-compile
tools/merge.pl gui/evan.py > bin/evan
chmod +x bin/evan

echo Generating documentation...
mkdir -p docs
tools/xdocs.pl lib/EVAN > docs/reference.json

mkdir -p analysis
