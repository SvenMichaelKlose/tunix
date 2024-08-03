#!/bin/sh

set -e

DATE=`date +%Y-%m-%d`
make allworlds NDEBUG=1 $@
cd src/bin/lisp/doc && ./md2pdf.sh && cd -
cp src/bin/lisp/doc/manual.pdf tunix-lisp.pdf
cp src/bin/lisp/doc/manual.md tunix-lisp.md
rm tunix-lisp.unix.d64
rm -f tunix.cbm.$DATE.zip
zip -o tunix.cbm.$DATE.zip tunix-lisp.md tunix-lisp.pdf *.d64
