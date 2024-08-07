# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### TUNIX Lisp

#### Added

- Unix: Environment file "unix.lisp".
- Unix: Built-in function "time" and constant "+bps+".
- Compile-time option TEST enables all tests at program initialization.
- EXPERIMENTAL!: Compression of conses in user-triggered garbage collection.
  (See manual for details.)
- CHECK\_OBJ\_POINTERS at compile-time will enable quick sanity checks that
  is suitable for use on small machines.  On TARGET\_UNIX it's thorough
  and slow, but easy to reget if not used for testing.
- Fixed GC trigger.  Did not take end-of-heap marker into account.
- Exit interpreter if out of heap.
- Detect if relocation table is full when switching to the next heap.
- Add PARANOID relocation table overflow check to GC sweep phase.
