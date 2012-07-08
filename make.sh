#!/bin/sh

if [ ! -d build ]; then
	mkdir -p build
	cd build
	cmake ..
	cd ..
fi

cd build
make

