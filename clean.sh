#!/bin/sh

set -e

EVANROOT=`pwd`

rm -rf analysis bin docs gui/__pycache__

cd $EVANROOT/lib
cabal clean

cd $EVANROOT/www
cabal clean

cd $EVANROOT/tools/mkref
cabal clean

