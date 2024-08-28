The TUNIX Project
=================

⚠️ **This project is currently under heavy development and
has not been officially released yet.**

# Overview

This project attempts to create an operating system for
minimalistic computers that is portable, self-hosting, and
multi-tasking, complete with a comprehensive set of
applications.

TUNIX Lisp is under heavy construction at the moment.

* [Release changes](./Changelog.md)
* [Build](BUILD.md)
* [Discuss](https://github.com/SvenMichaelKlose/tunix/discussions)
  (Be the first!)

## Targets

All targets require a disk drive of some sort, until images
can be created by the host or other targets and then saved
to other media, like tape or ROM, or within the Lisp program
itself.

| Target            | Requirements | Limits           |
|-------------------|--------------|------------------|
| Commodore C128    |              |                  |
| Commodore C16     |              | No debugger.     |
| Commodore C64     |              |                  |
| Commodore Plus/4  |              |                  |
| Commodore VIC-20  | +37K RAM.    |                  |

## Untested targets

| Target            |                                     |
|-------------------|-------------------------------------|
| Apple II          | Full memory expansion required.     |
| Apple II enhanced |                                     |

## Broken targets

| Commodore PET     | SYNTAX ERROR on program start.      |
| Generic Z80 CP/M  | Need help with z88dk compiler!      |

# Manuals

## TUNIX Lisp (glowing hot!)

Currently for Commodore home computers (C128, C16, C64,
Plus/4, VIC-20), and any modern Unixoid of your choice.

* [TUNIX Lisp](src/bin/lisp/doc/manual.md)
* [Bytecode compiler roadmap](src/bin/lisp/doc/compiler.md)
* [Future enhancements](src/bin/lisp/doc/future-enhancements.md)

## Libraries

There are [quite a lot for the VIC-20](src/lib/) but this
portale and embedded database will make a difference:

* [BielefeldDB](src/lib/bdb/README.md)

## Commodore VIC-20

* [TUNIX VIC-20 kernel](src/sys/kernel/doc/index.md)
* [VI editor](src/bin/vi/README.md)
* [Console](src/bin/cbm-console/README.md)
* [UltiFS filesystem](src/drv/ultifs/README.md)

