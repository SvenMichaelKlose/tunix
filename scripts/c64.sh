make worldclean world TARGET=c64 "$@"
pushd tunix/c64
x64 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd
