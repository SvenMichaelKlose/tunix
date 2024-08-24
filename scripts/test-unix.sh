#!/bin/sh

set -e

VERBOSE="-DVERBOSE_LOAD -DVERBOSE_DEFINES"
PARANOID="$VERBOSE -DEXIT_FAILURE_ON_ERROR -DLOAD_ALL -DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOW_CHECKS -DGCSTACK_UNDERFLOW_CHECKS -DTAGSTACK_OVERFLOW_CHECKS -DTAGSTACK_UNDERFLOW_CHECKS"
SIM65=`pwd`/src/contrib/cc65/bin/sim65

test_sim6502 () {
    make clean world TARGET=sim6502 LISP_FLAGS="$1" $2
    cd tunix/sim6502/ && (printf "(isave \"image\")" | $SIM65 lisp) ; cd -
    cd tunix/sim6502/ && (printf "\n" | $SIM65 lisp) ; cd -
}

test_unix () {
    make clean world TARGET=unix LISP_FLAGS="$1" $2
    cd tunix/unix/ && (printf "(isave \"image\")(iload \"image\")" | ./lisp) ; cd -
}

test_unix "-DEXIT_FAILURE_ON_ERROR"
test_unix "$PARANOID"
test_unix "-DGC_STRESS"
test_unix "$PARANOID -DCOMPRESSED_CONS"
test_unix "$PARANOID -DNO_ONERROR"
test_unix "$PARANOID -DNO_DEBUGGER"
test_unix "-DNAIVE"

test_sim6502 "$PARANOID"
test_sim6502 "$VERBOSE -DNDEBUG"
test_sim6502 "$PARANOID -DCOMPRESSED_CONS"
#test_sim6502 "$PARANOID -DNO_ONERROR"
test_sim6502 "$PARANOID -DNO_DEBUGGER"
test_sim6502 "$VERBOSE -DNAIVE"
