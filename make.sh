#!/bin/sh

mkdir -p compiled
mkdir -p compiled/lib
sbcl --noinform --core bender/bender make.lisp
rm g.zip
cd compiled && zip -r ../g.zip *
