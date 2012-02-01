#!/bin/sh

set -e

rm -rf analysis bin docs gui/__pycache__

pushd . > /dev/null
cd lib
cabal clean
popd > /dev/null

pushd . > /dev/null
cd www
cabal clean
popd > /dev/null

