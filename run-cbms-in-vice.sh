#!/bin/sh

set -e

make clean all TARGET=c128 && x128 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.c128.d64
make clean all TARGET=c16 && xplus4 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.c16.d64
make clean all TARGET=c64 && x64 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.c64.d64
make clean all TARGET=plus4 && xplus4 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.plus4.d64
make clean all TARGET=vic20 && xvic -moncommands src/bin/lisp/lisp.lbl tunix-lisp.vic20.d64
