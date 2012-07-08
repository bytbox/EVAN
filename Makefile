all: build

build: out
	cd out && make -s

check:
	cd out && make -s check

clean:
	${RM} -r out

out:
	if [ ! -d out ]; then mkdir -p out && cd out && cmake ..; fi

.PHONY: all build clean

