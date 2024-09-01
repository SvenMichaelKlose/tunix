#!/bin/sh

set -e

pushd tunix/vic20
xvic -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd

pushd tunix/c64
x64 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd

pushd tunix/c128
x128 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd

pushd tunix/c16
xplus4 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd

pushd tunix/plus4
xplus4 -attach8rw -autostartprgmode 0 -moncommands lisp.lbl lisp
popd
