#!/bin/sh

set -e

build_and_run () {
    echo "#### Testing $@"
    #make clean allworlds $@
    make clean all TARGET=unix LOAD_ALL=1
    cd tunix/unix/ && (echo | ./lisp) ; cd -
}

test_options () {
    build_and_run $@
    build_and_run COMPRESSED_CONS=1 VERBOSE_COMPRESSED_CONS=1 $@
    build_and_run NO_ONERROR=1 $@
    build_and_run NO_DEBUGGER=1 $@
    build_and_run NO_IMAGES=1 $@
}

test_diagnostics () {
    test_options NDEBUG=1 $@
    test_options $@
    test_options TEST=1 $@
    test_options PARANOID=1 $@
    test_options CHECK_OBJ_POINTERS=1 $@
}

test_diagnostics

# Stealth version
build_and_run NAIVE=1 $@
build_and_run NAIVE=1 COMPRESSED_CONS=1 $@

# Release version
build_and_run VERBOSE_LOAD=1 COMPRESSED_CONS=1 NDEBUG=1 $@
