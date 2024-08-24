#!/bin/sh

set -e

DATE=`date +%Y-%m-%d`
make worldclean world TARGET=vic20 LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES -DPARANOID -DCHECK_OBJ_POINTERS -DGCSTACK_OVERFLOW_CECKS -DTAGSTACK_OVERFLOW_CHECKS" $@
make worldclean world TARGET=c16 LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES -DPARANOID -DCHECK_OBJ_POINTERS -DGCSTACK_OVERFLOW_CECKS -DTAGSTACK_OVERFLOW_CHECKS -DNO_DEBUGGER" $@
for target in c128 c64 pet plus4 sim6502; do
    make worldclean world TARGET=$target LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES -DPARANOID -DSLOW -DCHECK_OBJ_POINTERS -DGCSTACK_OVERFLOW_CECKS -DTAGSTACK_OVERFLOW_CHECKS" $@
done
