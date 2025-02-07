#!/bin/sh

set -e

make worldclean world TARGET=unix COPTFLAGS="-O0 -g" "$@"
pushd tunix/unix
gdb --ex run --args lisp
popd
