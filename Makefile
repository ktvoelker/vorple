
.PHONY: all deploy clean

SOURCES=$(wildcard *.hs)

all: deploy

deploy: test
	cp test /srv/httpd/test/
	apachectl graceful

test: $(SOURCES)
	ghc --make -o test $(SOURCES)

clean:
	-rm *.o *.hi test

