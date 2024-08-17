#!/bin/sh

set -e

PARANOID_FLAGS="-DGC_STRESS -DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOWCHECKS -DGCSTACK_UNDERFLOWCHECKS -DTAGSTACK_OVERFLOWCHECKS -DTAGSTACK_UNDERFLOWCHECKS"

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
    test_options "$PARANOID_FLAGS" "GC_STRESS=1"
}

build_and_run "-DNAIVE"

build_and_run "$PARANOID_FLAGS"
cd tunix/unix/ && (echo "(isave \"image\")\n(iload \"image\")\n" | ./lisp) ; cd -
# Not loadable.  Needs relocating.
#cd tunix/unix/ && (echo | ./lisp) ; cd -

#stress_out
