#!/bin/sh

set -e

echo Testing EVAN haskell libraries...
pushd . > /dev/null
cd lib
cabal configure --enable-tests
cabal test
popd > /dev/null
