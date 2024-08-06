#!/bin/sh

set -e

xvic tunix-lisp.vic20.d64
x64 tunix-lisp.c64.d64
x128 tunix-lisp.c128.d64
xplus4 tunix-lisp.c16.d64
xplus4 tunix-lisp.plus4.d64
