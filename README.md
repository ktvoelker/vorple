
Vorple, a simple web framework for Haskell
==========================================

Vorple is a simple framework for web applications that just want to send and
receive JSON. Take a look at the [example](https://github.com/ktvoelker/vorple/blob/master/example/Simple.lhs) to see how it works!

Installation
------------

Vorple isn't on Hackage yet, so you'll have to clone it from here. I highly recommend
using [cabal-dev](https://github.com/creswick/cabal-dev) to install Vorple's
dependencies in a sandbox.

    cabal install cabal-dev  # if you don't have cabal-dev yet
    git clone git://github.com/ktvoelker/vorple.git
    cd vorple
    cabal-dev install

If you want to run the test suite, you will also need my as-yet-unpublished
[cookie-jar package](https://github.com/ktvoelker/cookie-jar).

    cd ..
    git clone git://github.com/ktvoelker/cookie-jar.git
    cd vorple
    cabal-dev add-source ../cookie-jar
    cabal-dev configure --enable-tests
    cabal-dev test

