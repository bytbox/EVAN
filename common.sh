# common.sh
# Scans environment

EVANROOT=`pwd`
EVANBIN=$EVANROOT/bin

if which go > /dev/null 2>/dev/null; then
	GO=`which go`
fi

if which python3 > /dev/null 2>/dev/null; then
	PY=`which python`
fi
