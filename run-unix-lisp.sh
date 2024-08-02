#!/bin/sh

set -e

make clean all $@
# Causes SIGSEGV:
#cd src/bin/lisp && echo "(exit 0" | ./lisp
cd src/bin/lisp && gdb -ex run --args ./lisp
