#!/bin/sh

set -e

# For whatever reason without this cc65 won't have its
# libraries built.
make host

PARANOID="-DLOAD_ALL -DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOWCHECKS -DGCSTACK_UNDERFLOWCHECKS -DTAGSTACK_OVERFLOWCHECKS -DTAGSTACK_UNDERFLOWCHECKS"
SIM65=`pwd`/src/contrib/cc65/bin/sim65

test_sim6502 () {
    make clean world TARGET=sim6502 LISP_FLAGS="$1" $2
    cd tunix/sim6502/ && (printf "(isave \"image\")" | $SIM65 lisp) ; cd -
    cd tunix/sim6502/ && (printf | $SIM65 lisp) ; cd -
}

test_unix () {
    make clean world TARGET=unix LISP_FLAGS="$1" $2
    cd tunix/unix/ && (printf "(isave \"image\")(iload \"image\")" | ./lisp) ; cd -
}

# TODO: -DCHECK_OBJ_POINTERS on sim65.
test_sim6502 "-DLOAD_ALL"
test_sim6502 "-DLOAD_ALL -DNDEBUG"
test_sim6502 "-DLOAD_ALL -DCOMPRESSED_CONS"
test_sim6502 "-DLOAD_ALL -DNO_ONERROR"
test_sim6502 "-DLOAD_ALL -DNO_DEBUGGER"
test_sim6502 "-DLOAD_ALL -DNAIVE"

test_unix
test_unix "$PARANOID"
test_unix "-DGC_STRESS $PARANOID_"
test_unix "-DCOMPRESSED_CONS $PARANOID"
test_unix "-DNO_ONERROR $PARANOID"
test_unix "-DNO_DEBUGGER $PARANOID"
test_unix "-DNAIVE"

./make-development-versions.sh
