#!/bin/sh

mkdir -pv compiled
sbcl --noinform --core bender/bender make.lisp
rm -v g.zip
cp -v README.md compiled
cd compiled && zip -r ../g.zip *
