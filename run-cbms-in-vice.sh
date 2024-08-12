#!/bin/sh

set -e

xvic -attach8rw tunix-lisp.vic20.d64
x64 -attach8rw tunix-lisp.c64.d64
x128 -attach8rw tunix-lisp.c128.d64
xplus4 -attach8rw tunix-lisp.c16.d64
xplus4 -attach8rw tunix-lisp.plus4.d64
