all: build

build: out
	@cd out && ${MAKE} -s

full: out
	@cd out && ${MAKE} -s full

doc: out
	@cd out && ${MAKE} -s doc-html

check: out
	@cd out && ${MAKE} -s check

clean:
	@${RM} -r out

out:
	@if [ ! -d out ]; then mkdir -p out && cd out && cmake ..; fi

.PHONY: all build clean check

