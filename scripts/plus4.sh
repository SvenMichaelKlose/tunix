make clean world TARGET=plus4
pushd tunix/plus4
xplus4 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd
