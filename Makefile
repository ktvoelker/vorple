
.PHONY: all deploy clean

SOURCES=$(shell find . -name '*.hs')

all: deploy

deploy: bin/main bin/listen
	cp bin/* /srv/httpd/test/bin/
	cp -r static/* /srv/httpd/test/static/
	apachectl graceful

bin/main: $(SOURCES)
	mkdir -p bin
	ghc --make -o bin/main Main

bin/listen: $(SOURCES)
	mkdir -p bin
	ghc --make -main-is Listen -o bin/listen Listen

clean:
	-rm $(shell find . -name '*.o') $(shell find . -name '*.hi')
	-rm bin/*
	-rmdir bin

