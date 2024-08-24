Building TUNIX
==============

Before doing anything else after cloning this repository,
please do this first to fetch all third-party code:

~~~sh
git submodule update --init --recursive
~~~

⚠️ **I've got to apologize right away but you cannot
yet use multiple jobs for builds.**

After that simply build all targets with:

~~~sh
make host   # (Not required.)
make allworlds
~~~

Directories for all target are then to be found in
directory 'tunix'.

You can also select a particular TARGET, e.g.:

~~~sh
make world TARGET=vic20
~~~

Giving you new files in directory 'tunix/vic20'.
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

There are also incomplete targets that need some love:

| Target   | Description                   |
|----------|-------------------------------|
| cpm      | Generic CP/M (Z80-CPU)        |
| zx       | Sinclair ZX Spectrum          |
