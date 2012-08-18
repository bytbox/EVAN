all: build
	@cd build && ${MAKE} -s

full: build
	@cd build && ${MAKE} -s full

doc: build
	@cd build && ${MAKE} -s doc-html

check: build
	@cd build && ${MAKE} -s check

prof: build.prof
	@cd build.prof && ${MAKE} -s

cov: build.cov
	@cd build.cov && ${MAKE} -s check

clean:
	@${RM} -r build build.prof build.cov

build:
	@if [ ! -d build ]; then mkdir -p build && cd build && cmake ..; fi

build.prof:
	@if [ ! -d build.prof ]; then mkdir -p build.prof && cd build.prof && cmake -DPROFILE=1 ..; fi

build.cov:
	@if [ ! -d build.cov ]; then mkdir -p build.cov && cd build.cov && cmake -DCOVERAGE=1 ..; fi

.PHONY: all clean check prof cov

