#!/bin/sh

set -e

VERBOSE="-DVERBOSE_LOAD -DVERBOSE_DEFINES"
PARANOID="$VERBOSE -DEXIT_FAILURE_ON_ERROR -DCHECK_OBJ_POINTERS -DTEST -DPARANOID"
SIM65=`pwd`/src/contrib/cc65/bin/sim65

test_unix () {
    make worldclean world TARGET=unix LISP_FLAGS="$1" $2
    cd tunix/unix/ ; (printf "" | ./lisp) ; cd -
}

test_sim6502 () {
    make clean world TARGET=sim6502 LISP_FLAGS="$1" $2
    cd tunix/sim6502/ ; (printf "" | $SIM65 lisp) ; cd -
}

test_unix "$PARANOID -DGC_STRESS"

tests () {
    $1 "$PARANOID" NDEBUG=1
    $1 "$PARANOID -DCOMPRESSED_CONS"
    #$1 "$PARANOID -DNO_ONERROR"
    #$1 "$PARANOID -DNO_DEBUGGER"
    # TODO: NO_BUILTIN_NCONC NO_BUILTIN_APPEND 
    for i in NO_BUILTIN_CHAR_AT NO_BUILTIN_NTHCDR NO_BUILTIN_SUBSEQ; do
        $1 "$PARANOID -D$i"
    done
    # TODO: NO_BUILTIN_GROUP_SYMBOL_NAME
    for i in NO_BUILTIN_GROUP_BITOPS NO_BUILTIN_GROUP_DIRECTORY NO_BUILTIN_GROUP_IMAGE NO_BUILTIN_GROUP_RAW_ACCESS; do
        $1 "$PARANOID -D$i"
    done
    $1 "-DNAIVE -DEXIT_FAILURE_ON_ERROR $VERBOSE"
}

tests test_unix
tests test_sim6502
