#!/bin/sh

set -e

build_and_run () {
    make clean all TARGET=unix LISP_FLAGS="$1" $2
    cd tunix/unix/ && (echo | ./lisp) ; cd -
}

test_options () {
    build_and_run "$1" $2
    build_and_run "-DCOMPRESSED_CONS $1" $2
    build_and_run "-DNO_ONERROR $1" $2
    build_and_run "-DNO_DEBUGGER $1" $2
    build_and_run "-DNO_IMAGES $1" $2
}

stress_out () {
    test_options "-DGC_STRESS -DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOWCHECKS -DGCSTACK_UNDERFLOWCHECKS -DTAGSTACK_OVERFLOWCHECKS -DTAGSTACK_UNDERFLOWCHECKS" "GC_STRESS=1"
}

stress_out

build_and_run "-DNAIVE"
build_and_run "-DNAIVE -DCOMPRESSED_CONS"

# Test image and be paranoid about it.
#build_and_run "-DVERBOSE_LOAD -DCOMPRESSED_CONS -DVERBOSE_COMPRESSED_CONS -DPARANOID -DCHECK_OBJ_POINTERS $@"
#cd tunix/unix/ && (echo "(isave \"image\")" | ./lisp) ; cd -
#cd tunix/unix/ && (echo | ./lisp) ; cd -

build_and_run "-DNDEBUG"
