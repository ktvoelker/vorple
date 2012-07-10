
.PHONY: all configure build deploy clean

TARGET=/srv/httpd/test

all: deploy

deploy: build
	cp dist/build/vorple-example-main/vorple-example-main $(TARGET)/bin/main
	cp dist/build/vorple-example-listen/vorple-example-listen $(TARGET)/bin/listen
	cp -r static/* $(TARGET)/static/
	apachectl graceful

configure:
	cabal configure

build: configure
	cabal build

clean:
	cabal clean

