Building TUNIX
==============

# Getting a compiled release instead

First off: you can also
[grab a binary release online](https://github.com/SvenMichaelKlose/tunix/releases)
for the sniff test.

# Checking everything out

Before doing anything else after cloning this repository,
please do this first to fetch all third-party code:

~~~sh
git submodule update --init --recursive
~~~

# Building everything

⚠️ **I've got to apologize right away but you cannot
yet use multiple jobs for builds.**

After that simply build all targets with:

~~~sh
make host   # (Not required.)
make allworlds
~~~

Directories for all target are then to be found in
directory 'tunix'.

# Building for particular targets

You can also select a particular TARGET, e.g.:

~~~sh
make worldclean world TARGET=vic20
~~~

Giving you new files in directory 'tunix/vic20'.

For TARGET you have these to choose:

| Target   | Description                                 |
|----------|---------------------------------------------|
| c128     | Commodore C128                              |
| c16      | Commodore C16                               |
| c64      | Commodore C64                               |
| pet      | Commodore PET (doesn't start)               |
| plus4    | Commodore Plus/4                            |
| sim6502  | cc65's sim65                                |
| unix     | Host machine (for GCC-compatible toolchain) |
| vic20    | Commodore C128                              |

There are also incomplete targets that could us some love:

| Target   | Description                   |
|----------|-------------------------------|
| cpm      | Generic CP/M (Z80-CPU)        |
| zx       | Sinclair ZX Spectrum          |

# Running the Lisp

## Run on Unix

~~~sh
cd tunix/unix
./lisp
~~~

## Run in VICE (VersatIle Commodore Emulator)

Step into one of the subdirectories in directory 'tunix'
and launch the version of VICE, depending on the platform
you picked, e.g.:

~~~sh
cd tunix/vic20
xvic -attach8rw -autostartprgmode 0 lisp
~~~

You can also run all CBM machines, one by one, using shell
script
[scripts/run-cbms-in-vice.sh](scripts/run-cbms-in-vice.sh).

# Building with other feature sets

Sooner or later you'll want to apply build and compile-time
options, e.g.:

~~~sh
make world TARGET=vic20 NDEBUG=1 LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES"
~~~

where 'NDEBUG=1' will exclude all extra checks you just want
around if the current state of the project is unstable, e.g.
when developing, but have an notable impact on performace.
A complete set of compile-time options is (almost)
guaranteed to be found in the head of file
'src/lib/lisp/liblisp.h'.

# Building for (interpeter) development

If you want to play around with the interpreter code on
platforms that don't have a decent debugger, better
fly with seat belt and parachute[^nodebugger]:

[^nodebugger]
  There is some toy IDE with a plug-in for VICE but that
  requires registering on a world-dominating, damaging
  company's web site.  Personally, I'd prefer to die like a
  dog instead.

~~~sh
make worldclean world TARGET=vic20 LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES -DVERBOSE_GC -DTEST -DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOW_CHECKS -DGCSTACK_UNDERFLOW_CHECKS -DTAGSTACK_OVERFLOW_CHECKS -DTAGSTACK_UNDERFLOW_CHECKS"
~~~

On Unix you can also add '-DGC\_STRESS' to trap lost
pointers, but that's very time-consuming.
If push comes to shove VERBOSE\_READ and VERBOSE\_EVAL
may be of help and there are even more diagnostics worth
a look.

## Debugging on Unices

For debugging on Unices (with 'gdb' for example), don't miss
out on disabling the C compiler's optimization flags, or the
debugger won't work properly with the code having been
optimized to something else:

~~~sh
make worldclean world TARGET=unix COPTFLAGS="-O0 -g" LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES -DVERBOSE_GC -DTEST -DCHECK_OBJ_POINTERS -DTEST -DPARANOID -DGCSTACK_OVERFLOW_CHECKS -DGCSTACK_UNDERFLOW_CHECKS -DTAGSTACK_OVERFLOW_CHECKS -DTAGSTACK_UNDERFLOW_CHECKS"
~~~

You're more than welcome to ask for help and discuss things
on
[Github discussions](https://github.com/SvenMichaelKlose/tunix/discussions).
