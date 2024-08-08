# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### TUNIX Lisp

#### Fixed

- Fixed GC trigger.  Did not take end-of-heap marker into account.
- Detect if relocation table is full when switching to the next heap.

#### Added

- Unix: Environment file "unix.lisp".
- Unix: Built-in function "time" and constant "+bps+".
- Compile-time option TEST enables all tests at program initialization.
- Compression of conses in user-triggered garbage collection.
  (See manual for details.)
- CHECK\_OBJ\_POINTERS at compile-time will enable quick sanity checks that
  is suitable for use on small machines.  On TARGET\_UNIX it's thorough
  and slow, but easy to regret if not enabled during tests.
- Add PARANOID relocation table overflow check to GC sweep phase.
- Option VERBOSE\_COMPRESSED\_CONS for diagnostic printing a 'C' for
  each compressed cons.
- Breakpoints on user-defined procedures. (Symbol list "\*b\*".)
- Debugger keeps its current return value in symbol "\*r\*".

#### Changed

- Interpreter exits if out of heap instead of going vodka.  This is temporary.
- Debugger command 'p': Does not modify the return value.

### libsimpleio

#### Fixed

- File errors are reset.
