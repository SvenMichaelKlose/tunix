make worldclean world TARGET=c128 "$@"
pushd tunix/c128
x128 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd
