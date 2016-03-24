#!/bin/sh

mkdir -pv compiled
mkdir -pv obj
sbcl --noinform --core bender/bender make.lisp
rm -v g.zip
cp -v README.md compiled
cd compiled && zip -r ../g.zip *
