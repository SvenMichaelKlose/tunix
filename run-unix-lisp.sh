#!/bin/sh

set -e

make clean all $@
cd src/bin/lisp && gdb -ex run --args ./lisp
