#!/bin/sh

set -e

for target in c128 c16 pet vic20; do
    make clean world TARGET=$target NDEBUG=1 LISP_FLAGS="-DCOMPRESSED_CONS -DVERBOSE_LOAD=1 -DVERBOSE_DEFINES=1"
done
for target in c64 plus4 unix; do
    make clean world TARGET=$target NDEBUG=1 LISP_FLAGS="-DVERBOSE_LOAD=1 -DVERBOSE_DEFINES=1"
done
cd src/bin/lisp/doc && ./md2pdf.sh && cd -
cp src/bin/lisp/doc/manual.pdf tunix/tunix-lisp.pdf
cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
rm -f $1
zip -r -o $1 tunix
