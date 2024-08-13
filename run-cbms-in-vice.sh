#!/bin/sh

set -e

cd tunix/vic20 && xvic -attach8rw -autostartprgmode 0 lisp
cd tunix/c64 && x64 -attach8rw -autostartprgmode 0 lisp
cd tunix/c128 && x128 -attach8rw -autostartprgmode 0 lisp
cd tunix/c16 && xplus4 -attach8rw -autostartprgmode 0 lisp
cd tunix/plus4 && xplus4 -attach8rw -autostartprgmode 0 lisp
