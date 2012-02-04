#!/bin/sh

# test.sh
# Runs all available tests.

set -e

echo Testing EVAN haskell libraries...
pushd . > /dev/null
cd evanlib
cabal configure --enable-tests
cabal build
cabal test
popd > /dev/null
