#!/bin/sh

set -e

make clean all GC_STRESS=1 $@
# Causes SIGSEGV:
#cd src/bin/lisp && echo "(exit 0" | ./lisp
cd src/bin/lisp && gdb -ex run --args ./lisp
