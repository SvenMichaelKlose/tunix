#!/bin/sh

set -e

xvic -moncommands src/bin/lisp/lisp.lbl tunix-lisp.vic20.d64
x64 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.c64.d64
x128 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.c128.d64
xplus4 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.c16.d64
xplus4 -moncommands src/bin/lisp/lisp.lbl tunix-lisp.plus4.d64
