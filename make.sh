#!/bin/sh

set -e

mkdir -pv compiled
sbcl --noinform --core bender/bender make.lisp
cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
make
rm -fv g.zip compiled/charset.lst
cp -v README.md compiled
cd compiled && zip -r ../g.zip *
