The TUNIX Project
=================

⚠️ **This project is currently under heavy development and
has not been officially released yet.**

# Overview

This project attempts to create an operating system for
minimalistic computers that is portable, self-hosting, and
multi-tasking, complete with a comprehensive set of
applications.

## TUNIX Lisp (glowing hot!)

TUNIX Lisp is the centre of it all and under heavy
construction.

* [TUNIX Lisp](src/bin/lisp/doc/manual.md)
* [Bytecode compiler roadmap](src/bin/lisp/doc/compiler.md)
* [Future enhancements](src/bin/lisp/doc/future-enhancements.md)

* [Wiki](https://github.com/SvenMichaelKlose/tunix/wiki)
* [Release changes](./Changelog.md)
* [Build](BUILD.md)
* [Discuss](https://github.com/SvenMichaelKlose/tunix/discussions)
  (Be the first!)

## Targets

All targets require a disk drive of some sort.

| Target            | Requirements | Limits             |
|-------------------|--------------|--------------------|
| Commodore C128    |              | No images/debugger |
| Commodore C16     |              | Defunct.           |
| Commodore C64     |              |                    |
| Commodore plus/4  |              |                    |
| Commodore VIC-20  | +37K RAM.    | No images/debugger |

## Untested targets

| Target            |                                     |
|-------------------|-------------------------------------|
| Apple II          | Full memory expansion required.     |
| Apple II enhanced |                                     |
| CP/M              |                                     |

For these targets binaries are compiled but no-one went
through the trouble making disk images and running them in
an emulator.  Almost guaranteed to not work.

## Broken targets

| Target            |                                     |
|-------------------|-------------------------------------|
| Commodore PET     | SYNTAX ERROR on program start.      |

# Other Parts of TUNIX

## Libraries

There are [quite a lot for the VIC-20](src/lib/) but this
portale and embedded database will make a difference:

* [BielefeldDB](src/lib/bdb/README.md)

## Commodore VIC-20

* [TUNIX VIC-20 kernel](src/sys/kernel/doc/index.md)
* [VI editor](src/bin/vi/README.md)
* [Console](src/bin/cbm-console/README.md)
* [UltiFS filesystem](src/drv/ultifs/README.md)

