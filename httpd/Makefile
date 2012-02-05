all:
	if [ ! -d dist ]; then cabal configure; fi
	cabal build
	cp dist/build/evan-httpd/evan-httpd .

clean:
	cabal clean

.PHONY: all
