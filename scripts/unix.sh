#!/bin/sh

set -e

make worldclean world TARGET=unix "$@"
pushd tunix/unix
gdb --ex run --args lisp
popd
