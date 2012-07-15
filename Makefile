all: build

build: out
	@cd out && make -s

full: out
	@cd out && make -s full

doc: out
	@cd out && make -s doc-html

check: out
	@cd out && make -s check

clean:
	@${RM} -r out

out:
	@if [ ! -d out ]; then mkdir -p out && cd out && cmake ..; fi

.PHONY: all build clean check

