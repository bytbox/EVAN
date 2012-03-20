#!/bin/sh

# test.sh
# Runs all available tests.

set -e
EVANROOT=`pwd`

echo Testing EVAN haskell libraries...
cd $EVANROOT/evanlib
cabal configure --enable-tests
cabal build
cabal test

