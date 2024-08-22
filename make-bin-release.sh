#!/bin/sh

set -e

make allworlds NDEBUG=1 VERBOSE_LOAD=1 VERBOSE_DEFINES=1
cd src/bin/lisp/doc && ./md2pdf.sh && cd -
cp src/bin/lisp/doc/manual.pdf tunix/tunix-lisp.pdf
cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
rm -f $1
zip -r -o $1 tunix
