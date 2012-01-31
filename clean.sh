#!/bin/sh

set -e

rm -r analysis bin

pushd . > /dev/null
cd lib
cabal clean
popd > /dev/null
