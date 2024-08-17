#!/bin/sh

set -e

build_and_run () {
    echo "#### Testing $@"
    make clean all TARGET=unix LISP_FLAGS="-DGC_STRESS -DCHECK_OBJ_POINTERS -DTEST -DPARANOID$@"
    cd tunix/unix/ && (echo | ./lisp) ; cd -
}

test_options () {
    build_and_run "$@"
    build_and_run "-DCOMPRESSED_CONS $@"
    build_and_run "-DCOMPRESSED_CONS -DVERBOSE_COMPRESSED_CONS $@"
    build_and_run "-DNO_ONERROR $@"
    build_and_run "-DNO_DEBUGGER $@"
    build_and_run "-DNO_IMAGES $@"
    build_and_run "-DVERBOSE_EVAL $@"
}

test_diagnostics () {
    test_options "$@"
}

test_diagnostics

# Stealth version.
build_and_run "-DNAIVE $@"
build_and_run "-DNAIVE -DCOMPRESSED_CONS $@"

# Test image and be paranoid about it.
#build_and_run "-DVERBOSE_LOAD -DCOMPRESSED_CONS -DVERBOSE_COMPRESSED_CONS -DPARANOID -DCHECK_OBJ_POINTERS $@"
#cd tunix/unix/ && (echo "(isave \"image\")" | ./lisp) ; cd -
#cd tunix/unix/ && (echo | ./lisp) ; cd -

# Stress out.
build_and_run "-DVERBOSE_LOAD -DCOMPRESSED_CONS -DVERBOSE_COMPRESSED_CONS -DPARANOID -DCHECK_OBJ_POINTERS -DGC_STRESS $@"

# Release version.
build_and_run "-DVERBOSE_LOAD -DNDEBUG $@"
