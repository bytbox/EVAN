#!/bin/sh

pushd . > /dev/null
cd json2hs
make
popd > /dev/null

mkdir -p bin
rm -f bin/*
ln -s `pwd`/json2hs/json2hs `pwd`/scripts/evan-compile bin
