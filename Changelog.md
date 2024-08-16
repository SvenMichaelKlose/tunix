# Changelog

All notable changes to this project will be documented in
this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [current]

### TUNIX Lisp debugger REPL

#### Changed

- Distinguish between error-fixing and stepping mode by
  printing context-sensitive labels.
- Block continuing unless an alternative for an erroraneous
  expression has been provided.

### TUNIX Lisp environment

- +V+ contains tag (if available) and short SHA.  It's
  printed when loading the environment.

## [v0.0.1]

Changes since this file has been created.

### General

#### Removed

- D64 images as the have become too small for some targets.
  Let's spare the trouble.

#### Added

- Directories with binaries to replace D64 images.

### TUNIX Lisp debugger

#### Fixed

- Keep highlighting of current expression when evaluating
  argument of short command.

#### Added

- Breakpoints (new short commands).
- Improved REPL return value handling.

### TUNIX Lisp

#### Fixed

- Fixed GC trigger.  Did not take end-of-heap marker into
  account.
- Detect if relocation table is full when switching to the
  next heap.
- Check if argument names are symbols.
- Fixed OPEN's write mode without NDEBUG.
- READ: Do not put 0 back into buffer on end of file.

#### Changed

- Interpreter exits if out of heap instead of going vodka.
  This is temporary.
- Debugger command 'p': Does not modify the return value.
- BUTLAST, COPY-LIST, REMOVE: Function to handle all three
  has been rewritten.
- REMOVE can handle atoms and dotted pairs.
- VALUE became SYMBOL-VALUE.

#### Added

- Unix: Environment file "unix.lisp".
- Unix: Built-in function "time" and constant "+bps+".
- Compile-time option TEST enables all tests at program
  initialization.
- Compression of conses in user-triggered garbage
  collection.  (See manual for details.)
- CHECK\_OBJ\_POINTERS at compile-time will enable quick
  sanity checks that is suitable for use on small machines.
  On TARGET\_UNIX it's thorough and slow, but easy to regret
  if not enabled during tests.
- NO\_CHECK\_CPU\_STACK to not check CPU stack on overflow.
- Add PARANOID relocation table overflow check to GC sweep
  phase.
- Option VERBOSE\_COMPRESSED\_CONS for diagnostic printing a
  'C' for each compressed cons.
- ERROR\_ARGNAME\_TYPE
- Macro WITH-GLOBAL to temporarily change the value of a
  symbol.
- Compile-time error on VERBOSE\_COMPRESSED\_CONS without
  COMPRESSED\_CONS.
- Built-in ISAVE and ILOAD to save and load the heap.  Can
  be disables by compile-time option NO\_IMAGES.
- Internal error: Print address of faulty pointer.
- Additional error info (expression) like lists of missing
  arguments.
- Faster checks if NOT on cc65-compiled platforms.
- SYMBOL-NAME and CHAR-AT return character value numbers of
  a symbol's name.
- Compile-time option VERBOSE\_DEFINES.  Not set by
  default.

### libsimpleio

#### Fixed

- File errors are reset.

#### Added

- simpleio\_open(): Check DOS status code.
- outhn(), outhb(), outhw(): Print hexadecimal nibble, byte
  or word.

### libsimpleio-cbm

- Fixed opening control channel #15.

### libsimpleio-stdlib

#### Fixed

- out(): Set err() if channel is invalid.
