#!/bin/sh

set -e

DATE=`date +%Y-%m-%d`
make allworlds LISP_FLAGS="-DVERBOSE_LOAD -DCOMPRESSED_CONS -DNDEBUG" $@
cd src/bin/lisp/doc && ./md2pdf.sh && cd -
cp README.md tunix/
cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
rm -f tunix.$DATE.zip
zip -r -o tunix.$DATE.zip tunix
