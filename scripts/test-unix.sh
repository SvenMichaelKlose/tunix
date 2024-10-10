#!/bin/sh

set -e

PARANOID="-DDEVELOPMENT -DEXIT_FAILURE_ON_ERROR -DTEST_INTERPRETER -DTEST_ENVIRONMENT"
SIM65=`pwd`/src/contrib/cc65/bin/sim65

test_unix () {
    make worldclean world TARGET=unix LISP_FLAGS="$1" $2
    cd tunix/unix/ ; (printf "" | ./lisp) ; cd -
}

test_sim6502 () {
    make clean world TARGET=sim6502 LISP_FLAGS="$1" $2
    cd tunix/sim6502/ ; (printf "" | $SIM65 lisp) ; cd -
}

tests () {
    $1 "-DTEST_ALL" NDEBUG=1
    $1 "$PARANOID -DTEST_ALL"
    $1 "$PARANOID -DTEST_ALL -DCOMPRESSED_CONS"
    $1 "-DNO_DEBUGGER"
    for i in NO_BUILTIN_CHAR_AT NO_BUILTIN_NTHCDR NO_BUILTIN_SUBSEQ; do
        $1 "$PARANOID -D$i"
    done
    for i in NO_BUILTIN_GROUP_DIRECTORY NO_BUILTIN_GROUP_RAW_ACCESS; do
        $1 "$PARANOID -D$i"
    done
}

tests test_unix
test_unix "$PARANOID -DGC_STRESS"
tests test_sim6502
