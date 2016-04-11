#!/bin/sh

mkdir -pv compiled
cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
sbcl --noinform --core bender/bender make.lisp
rm -v g.zip
cp -v README.md compiled
cd compiled && zip -r ../g.zip *
