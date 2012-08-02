all: build

build: out
	@cd out && ${MAKE} -s

full: out
	@cd out && ${MAKE} -s full

doc: out
	@cd out && ${MAKE} -s doc-html

check: out
	@cd out && ${MAKE} -s check

prof: out.prof
	@cd out.prof && ${MAKE} -s

cov: out.cov
	@cd out.cov && ${MAKE} -s check

clean:
	@${RM} -r out out.prof out.cov

out.prof:
	@if [ ! -d out.prof ]; then mkdir -p out.prof && cd out.prof && cmake -DPROFILE=1 ..; fi

out.cov:
	@if [ ! -d out.cov ]; then mkdir -p out.cov && cd out.cov && cmake -DCOVERAGE=1 ..; fi

out:
	@if [ ! -d out ]; then mkdir -p out && cd out && cmake ..; fi

.PHONY: all build clean check prof cov

