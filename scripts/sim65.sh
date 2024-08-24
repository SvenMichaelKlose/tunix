make worldclean world TARGET=sim6502 "$@"
pushd tunix/sim6502
sim65 lisp
popd
