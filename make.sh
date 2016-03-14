#!/bin/sh

rm -rf obj compiled
mkdir -p obj
mkdir -p compiled
mkdir -p compiled/lib
sbcl --noinform --core bender/bender make.lisp
