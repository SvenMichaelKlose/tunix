make clean world TARGET=vic20 "$@"
pushd tunix/vic20
xvic -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd
