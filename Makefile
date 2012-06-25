
.PHONY: all deploy clean

SOURCES=$(shell find . -name '*.hs')

all: deploy

deploy: test
	cp test /srv/httpd/test/bin/
	cp -r static/* /srv/httpd/test/static/
	apachectl graceful

test: $(SOURCES)
	ghc --make -o test $(SOURCES)

clean:
	-rm $(shell find . -name '*.o') $(shell find . -name '*.hi') test

