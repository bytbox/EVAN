#!/bin/sh

set -e

pushd . > /dev/null
cd gui
./genTools.pl
popd > /dev/null

pushd . > /dev/null
cd lib
cabal install
popd > /dev/null

mkdir -p bin
rm -f bin/*
ln -s `pwd`/json2hs/json2hs.py bin/json2hs
ln -s `pwd`/scripts/evan-compile bin/evan-compile
tools/merge.pl gui/evan.py > bin/evan
chmod +x bin/evan

mkdir -p analysis
