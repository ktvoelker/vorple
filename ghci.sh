#!/usr/bin/env bash
set -e
cd $(git rev-parse --show-toplevel)
exts=$(cat $(find . -name extensions.txt) | xargs -n1 -I{} echo -n " -X{}")
pkgs="-package-conf $(find . -name 'packages-*.conf') -no-user-package-conf"
srcs=-i.$(cat $(find . -name sources.txt) | xargs -n1 -I{} echo -n ":{}")
args=$(cat $(find . -name ghci.txt))
exec ghci $exts $pkgs $srcs $args
