Building TUNIX
==============

Before doing anything else after cloning this repository,
please do this first to fetch all third-party code:

~~~sh
git submodule update --init --recursive
~~~

After that simply build all targets with:

~~~sh
make allworlds
~~~

Binaries for all worlds are to be found in directory
'tunix'.

You can also select a particular TARGET, e.g.:

~~~sh
make world TARGET=vic20
~~~

These are the supported targets:

| Target   | Description                   |
|----------|-------------------------------||
| c128     | Commodore C128                |
| c16      | Commodore C16                 |
| c64      | Commodore C64                 |
| pet      | Commodore PET (doesn't start) |
| plus4    | Commodore Plus/4              |
| sim6502  | cc65's sim65                  |
| unix     | GCC-compatible toolchain      |
| vic20    | Commodore C128                |
