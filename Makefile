
.PHONY: all configure build install deploy clean

TARGET=/srv/httpd/test

all: install deploy

install: build
	-cabal-dev ghc-pkg unregister vorple
	cabal-dev install

deploy: build
	cp dist/build/vorple-example-main/vorple-example-main $(TARGET)/bin/main
	cp dist/build/vorple-example-listen/vorple-example-listen $(TARGET)/bin/listen
	cp -r static/* $(TARGET)/static/
	apachectl graceful

configure:
	cabal-dev configure

build: configure
	cabal-dev build

clean:
	cabal-dev clean

