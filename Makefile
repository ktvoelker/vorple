
.PHONY: all configure build install static deploy clean doc

TARGET=/srv/httpd/test

all: build doc

doc:
	cabal-dev haddock

install: build
	-cabal-dev ghc-pkg unregister vorple
	cabal-dev install

static:
	cp -r static/* $(TARGET)/static/

deploy: install static
	cp dist/build/vorple-example-main/vorple-example-main $(TARGET)/bin/main
	cp dist/build/vorple-example-listen/vorple-example-listen $(TARGET)/bin/listen
	apachectl graceful

configure:
	cabal-dev configure

build: configure
	cabal-dev build

clean:
	cabal-dev clean

