# Changelog

All notable changes to this project will be documented in
this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [current] - 2024-08-23

### TUNIX Lisp

#### Environment

##### Fixed

- Macro !? reimplemented and tests added.

##### Added

- Macro CASE: Evaluate conditionally by matching value.
- POSITION: Find position of object in list.
- SPLIT: Split list where object occurs, removing that object.


## [v0.0.5] - 2024-08-23

Most essential fixes.

### TUNIX Lisp

#### General

- Configure Comodore C128 to be able to load "all.lisp".
  (SLOW, COMPRESSED\_CONS).

#### Environment

##### Fixed

- APPEND with no arguments.

##### Added

- CDAR, MAPCAR, MAPAN


## [v0.0.4] - 2024-08-22

Heavy I/O reworks make this release worth a new checkout.

### libsimpleio

#### Changed

- Last input/output and put back chars are stored for each
  channel separately.
- eof() and err() are tied to their channels.

#### Added

- conio(): Returns waiting char or 0.

### TUNIX Lisp

#### Interpreter

##### Changed

- READ accepts negative numbers.
- READ with end of file checks on the spot.
- REPL has its own set of channels apart from the program.
- Default image is saved after environment has loaded.

##### Added

- CONIO: Unbuffered input.
- READ accepts char notation '\<char>' as promised in the
  manual.
- Compile-time option VERBOSE\_READ to have expression read
  by the REPL printed.

#### Debugger

##### Added

- TARGET\_UNIX: Highlighting with terminal reverse mode instead
  of triple chevrons, '<<<' and '>>>'.  There are kept for new
  targets.

## [v0.0.3] - 2024-08-18

### libsimpleio

### Fixed

- Building with clang.  Courtesy of pulluks.  Thanks!

### libsimpleio-cbm

- Print ASCII underscore '\_' as PETSCII graphics.

### TUNIX Lisp

#### Interpreter

##### Fixed

- Topmost REPL is not exited when a child debugger REPL
  issues a break.

##### Changed

- LOAD returns NIL on error, T otherwise.
- MEMBER uses EQL as the predicate to match number values.
  Otherwise working with character value lists would become
  a rather hairy issue.
- AND, LAST, OR, and SYMBOL issue an error for dotted pairs.

##### Added

- EXPERIMENTAL: Issue regular error when out of heap, but
  return to the current REPL and do a garbage collection
  before calling an ONERROR handler or the debugger.


## [v0.0.2] - 2024-08-17

### TUNIX Lisp

#### Build system

##### Changed

- Revived full stress test.
- src/config is not required any more.
- Added TARGET=sim6502 (cc65's simulator).

##### Removed

- oscar64 submodule.  It's never been used.

#### Debugger

##### Changed

- Make it more to the user if an error has to be fixed of if
  one is stepping through.
- Block continuing unless an alternative for an erroraneous
  expression has been provided.
- Step to next expression if alternative value has been
  provided.
- Tell if program is continuing.
- Evaluate alternative expression with program's I/O
  channels.

#### Interpreter

##### Changed

- READ breaks immediately on errors.
- REPL handles READ errors.
- Expects end of dotted pair.
- cc65: Smaller initializing parts for more heap.
- COPY-LIST, REMOVE, and BUTLAST do not support dotted pairs
  any more.

#### Environment

##### Added

- +V+ contains the Git tag.  It's printed when loading the
  environment.
- AWHEN assigns result of condition to local !.

### libsimpleio

#### Fixed

- fresh\_line() only if not NUL, CR or LF before.


## [v0.0.1] - 2024-08-15

Changes since this file has been created.

### General

#### Removed

- D64 images as the have become too small for some targets.
  Let's spare the trouble.

#### Added

- Directories with binaries to replace D64 images.

### TUNIX Lisp

#### Debugger

##### Fixed

- Keep highlighting of current expression when evaluating
  argument of short command.
- Short ommand 'p' does not modify the return value.

##### Added

- Breakpoints (new short commands).
- Improved REPL return value handling.
- Additional error info (expression) like lists of missing
  arguments.

#### Interpreter

##### Fixed

- Fixed GC trigger.  Did not take end-of-heap marker into
  account.
- Detect if relocation table is full when switching to the
  next heap.
- Fixed OPEN's write mode without NDEBUG.
- READ: Do not put 0 back into buffer on end of file.

##### Changed

- Exits if out of heap instead of going vodka.
  This is temporary.
- BUTLAST, COPY-LIST, REMOVE: Function to handle all three
  has been rewritten.
- REMOVE can handle atoms and dotted pairs.
- VALUE became SYMBOL-VALUE.
- Compile-time option VERBOSE\_DEFINES not set by default.
- Compile-time error if VERBOSE\_COMPRESSED\_CONS without
  COMPRESSED\_CONS.

##### Added

- Check if argument names are symbols as well as
  ERROR\_ARGNAME\_TYPE.
- Compression of conses in user-triggered garbage
  collection.  (Please see manual for details.)
- Add PARANOID relocation table overflow check to GC sweep
  phase.
- Built-in ISAVE and ILOAD to save and load the heap.  Can
  be disables by compile-time option NO\_IMAGES.
- Internal error: Print address of faulty pointer.
- Faster checks of NOT on 8/16-bit platforms.
- SYMBOL-NAME and CHAR-AT return character value numbers of
  a symbol's name.
- Unix also: Built-in function "time" and constant "+bps+".

###### Compile-time options

- NO\_CHECK\_CPU\_STACK to not check CPU stack on overflow.
- VERBOSE\_COMPRESSED\_CONS for diagnostic printing a 'C'
  for each compressed cons.
- Compile-time option TEST enables all tests at program
  initialization.
- CHECK\_OBJ\_POINTERS at compile-time will enable quick
  sanity checks that is suitable for use on small machines.
  On TARGET\_UNIX it's thorough and slow, but easy to regret
  if not enabled during tests.

#### Environment

- Macro WITH-GLOBAL to temporarily change the value of a
  symbol.
- Unix: Environment file "unix.lisp".

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
