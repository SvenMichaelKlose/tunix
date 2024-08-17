#!/bin/sh

set -e

PARANOID="-DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOWCHECKS -DGCSTACK_UNDERFLOWCHECKS -DTAGSTACK_OVERFLOWCHECKS -DTAGSTACK_UNDERFLOWCHECKS"
SIM65=`pwd`/src/contrib/cc65/bin/sim65

test_sim6502 () {
    make clean world TARGET=sim6502 LISP_FLAGS="$1" $2
    cd tunix/sim6502/ && (echo "(isave \"image\")" | $SIM65 lisp) ; cd -
    cd tunix/sim6502/ && (echo | $SIM65 lisp) ; cd -
}

test_unix () {
    make clean world TARGET=unix LISP_FLAGS="$1" $2
    cd tunix/unix/ && (echo -n "(isave \"image\")(iload \"image\")" | ./lisp) ; cd -
}

# TODO: -DCHECK_OBJ_POINTERS on sim65.
test_sim6502
test_sim6502 "-DNDEBUG"
test_sim6502 "-DCOMPRESSED_CONS"
test_sim6502 "-DNO_ONERROR"
test_sim6502 "-DNO_DEBUGGER"
test_sim6502 "-DNAIVE"

test_unix
test_unix "$PARANOID"
test_unix "-DGC_STRESS $PARANOID_"
test_unix "-DCOMPRESSED_CONS $PARANOID"
test_unix "-DNO_ONERROR $PARANOID"
test_unix "-DNO_DEBUGGER $PARANOID"
test_unix "-DNAIVE"
