make clean world TARGET=unix "$@"
pushd tunix/unix
gdb --ex run --args lisp
popd
