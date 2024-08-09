#!/bin/sh

set -e

make clean all $@
cd src/bin/lisp && ./lisp
