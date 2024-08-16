#!/bin/sh

set -e

make allworlds LISP_FLAGS="-DNDEBUG"
cd src/bin/lisp/doc && ./md2pdf.sh && cd -
cp README.md tunix/
cp src/bin/lisp/doc/manual.pdf tunix/tunix-lisp.pdf
cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
rm -f $1
zip -r -o $1 tunix
