# Changelog

All notable changes to this project will be documented in
this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [current]

### Lisp

#### Environment

##### Fixed

- CHAR-AT, SET-CHAR-AT: Don't crash on NULL pointer NILs.

##### Changed

- DOTEXPAND: Expand QUOTEd conses too.

##### Added

- IN-PACKAGE


## [v0.0.30] - 2024-12-10

### Lisp

#### Environment

##### Fixed

- REDUCE: No dot notation.

##### Added

- RESHAPE: Fold list to higher dimensions.

#### Interpreter

##### Fixed

- READ for dotted conses.


## [v0.0.29] - 2024-12-09

### libzp

- Faster zpw\_dec\_x (Courtesy of Michael Kircher.)
- Fixed zpd\_dec\_x

### Lisp

#### Environment

##### Fixed

- RESET!: Don't have it remove itself.

##### Changed

- WITH-QUEUE does not depend on MAKE-QUEUE and QUEUE-LIST anymore.

##### Added

- REDUCE
- INSERT, NINSERT: Non-(destructive) list inserts.
- COMPILE-ENV: First compiler passes under construction.

#### Interpreter

- READ: Fix reading literal cons "(car . cdr)".


## [v0.0.28] - 2024-11-16

### Lisp

#### Debugger REPL

##### Fixed

- Command 'n' (step over expression).


## [v0.0.27] - 2024-11-16

### libsimpleio-cbm

#### Fixed

- Output to IEC devices.

### Lisp

#### Environment

##### Fixed

- ERROR: Print all arguments like OUT.


## [v0.0.26] - 2024-11-14

### Lisp

#### Debugger REPL

##### Fixed

- Highlighting and type error messages.

#### Interpreter

##### Fixed

- Various debugger issues introduced recently.  (More do to.)
- Internal handling of NIL is more consistent, avoiding
  future bugs.
- GC: Faster relocation phase on 6502 targets.

##### Changed

- "cons" is now ".".
- Up to 10% better performance on SLOW configurations.
- REPL: Indent printed results by a space.
- Smaller stack sizes (due to recent update of eval()).
- TARGET\_VIC20: Use part of RAM123 as heap.
- \*EX\* now holds the expander, not MACROEXPAND to whose value
  \*EX\* is set now.
- SYMBOL: Re-use existing symbols unless the symbol is unnamed.

##### Added

- Compile-time option RESTART\_GC\_ON\_FULL\_RELOC: Makes the GC
  restart if it had to be stopped by a full relocation table.
  Turned off by default so save runs over already collected objects.

#### Environment

##### Fixed

- WITH-QUEUE: Don't return queue list on RETURN.
- Error handling reviewed.

##### Changed

- WITH-PROF returns number of bekloppies measured.
- MAKE-QUEUE is now a macro, so that it can be discarded.
- MESSAGE takes variable number of arguments (like OUT).

##### Added

- ACONS!
- Single inheritance, duck-typed object system based on associative
  lists.
- App "benchmark.lsp" gets basic GC time and calls per second.
- PAIRLIST to combine two lists to an associative one.
- COPY-LIST to copy associative lists.
- COPY-ALIST to copy associative lists.
- AS65-PARSE: READ-based 6502 assembly parser.
- PAD: Pad elements of a list.
- DOTEXPAND: Expands dot-notated symbols.  See manual for details.
- SOCKET-GET-DWORD, SOCKET-GET-WORD, SOCKET-GETN.

### libsimpleio

#### Added

- Each channel has its own vector table.

#### Removed

- outsn() is gone in favour of outm().


## [v0.0.25] - 2024-10-14

### Lisp

#### Interpreter

##### Changed

- Up to 30% better performance and smaller size of 8-bit versions.
- Faster breakpoint checks.

##### Fixed

- WITH-IN, WITH-OUT: Fix RETURN with missing BLOCK.
- Breakpoints revived.

##### Added

- SOCKET-GETC, SOCKET-PUTC: Char-wise socket I/O.
- Compile-time option: NIL\_NOT\_0 for REAL\_NIL on other address but 0.


## [v0.0.24] - 2024-10-13

### Lisp

#### Environment

##### Changed

- Discard function ISTART after saving boot image.

##### Interpreter

###### Fixed

- REPL: Fresh line before priting result.

###### Changed

- PRINT: No space after comma.

###### Added

- Basic UNIX sockets.  I/O is symbol-based.
  - SOCKET-CONNECT: Open an internetworking socket.
  - SOCKET-SEND: Send symbol name.
  - SOCKET-RECV: Read symbol up to maximum length.
  - SOCKET-BLOCK: Turn blocking mode on/off.
  - SOCKET-LISTEN: Start listening on port.
  - SOCKET-ACCEPT: Accept incoming connection.
  - SOCKET-CLOSE


## [v0.0.23] - 2024-10-12

### Lisp

#### Interpreter

##### Fixed

- Grand evaluator review, faster, using far less object stack.
- More descriptive messages on stack overflows.

##### Added

- Compile-time option FAST\_NIL for high-byte checks only.
- Compile-time option REAL\_NIL for more faster type checks.

##### Changed

- Don't say "Bye!" on exit.

#### Environment

##### Fixed

- AUTOLOAD: Don't load defined macros again.

##### Changed

- QUASIQUOTE conses less (uses NCONC instead of APPEND).


## [v0.0.22] - 2024-10-11

### libsimpleio-cbm

#### Fixed

- Control codes for TARGET\_UNIX.
- End-of-file shows up when trying to read past a file's end,
  not if the last byte has been read.

#### Added

- New code to get console flags.

### Lisp

#### Interpreter

##### Fixed

- REPL: Resets output channel correctly on return.
- REPL: Resets console before prompt.
- Debugger prints erroraneous value.
- Debugger resets console before prompt.
- APPLY: Complain if the only argument is not a list.
- REPL: Decrement debugger count correctly.
- REPL: Cleaned up switching channels.
- Avoid trashing rest of program on stack overflows.

##### Changed

- ERROR: PRINTs child expressions.
- ONERROR: Added error info argument.

##### Added

- CPU stack overflow checks.
- Compile-time option TEST\_ALL: Load "test-all.lsp" at end of boot.

#### Environment

##### Fixed

- Console functions.
- AUTOLOAD: Fix function type arguments.
- SYMBOL-NAME and SLENGTH accept built-ins.

##### Changed

- WITH: Variable initializers aren't in lists anymore.  Just one list
  that will be grouped by twos automatically.
- WITH is now LET.
- WITH* is now LET\*.
- AUTOLOAD: Be verbose by default (\*ALV?\* is T).

##### Added

- Simple console tests.
- Macro DO\*.
- Macro APROG1: Anaphoric version of macro PROG1, locally assigning the
  return value to "!".
- Macro WITH\*: Like macro WITH, it defines local variables sequentially,
  but allowing each variable to reference the ones defined earlier.
- DUP: Duplicates an element a number of times.
- Test MAKE-QUEUE and "!=".


## [v0.0.21] - 2024-10-05

### Lisp

#### libstdio-cbm

- Fix using current channel.  Used the last one for statuses.

#### Interpreter

##### Fixed

- RETURN breaks evaluation of argument lists as expected.

##### Changed

- Compile-time options VERBOSE\_LOAD and VERBOSE\_DEFINES have been
  inversed to NO\_VERBOSE\_LOAD and NO\_VERBOSE\_DEFINES.

##### Added

- Compile-time option NO\_ZEROPAGE for builds with cc65.

#### Environment

##### Fixed

- AUTOLOAD: Do not try to re-evaluate if file could not be loaded.
- STACK: Ensure fresh line before printing an entry.

##### Added

- CDDDR
- DOLIST-INDEXED
- LET-WHEN


## [v0.0.20] - 2024-10-02

### Lisp

#### Test

- Test VAR and FN.

#### Environment

##### Fixed

- MACRO: Don't trash output channel when verbose.

##### Changed

- All things console are in file "con.lsp".
- AUTOLOAD is mute in order to not mess up the screen.
  Simplified version, using macros.
- LS: Returns list instead of just printing it.

##### Added

- \*ALX\* contains name translations for AUTOLOAD.
- \*ALV?\* tells if AUTOLOAD should be verbose.
- AREMOVE, AREMOVE-IF

#### Interpreter

##### Fixed

- LOAD, VAR, FN: Don't trash output channel when verbose.
- VAR, FN: Didn't add to \*UNIVERSE\* with compile-time
  option VERBOSE\_DEFINES.
- PRINT: Also pad NIL (like all other symbols).

##### Added

- FREE: Return number of free bytes on heap.
- \*V?\* tells if definitions should be printed.
  (Requires compile-time option VERBOSE\_DEFINES.)
- Compiles for TARGET\_CPM.  Untested!

### libsimpleio-stdlib

- Unixoids: fix fetching cursor location.


## [v0.0.19] - 2024-09-24

### Lisp

#### Debugger

##### Fixed

- Step and REPL break.

#### Interpreter

##### Fixed

- APPEND: Show failing expression when skipping NILs.
- Option VERBOSE\_GC: Maintain output channel.

##### Added

- Compile-time option HOST\_DEBUGGER\_ON\_ERROR.
- SET-CHAR-AT to manipulate symbol names (to make bytecode
  functions).

##### Changed

- VIC-20: No debugger. (Out of heap.)

#### Environment

##### Fixed

- WITH-IN: Close temporary channel, not the old channel.
- MACRO: Doesn't assign the function expression to the
  macro name any more.

##### Changed

- POSITION: Iterative version.

##### Added

- POSITION-IF, SPLIT-IF.
- REQUIRE to load by name.


## [v0.0.18] - 2024-09-14

### Lisp

#### Interpreter

##### Fixed

- End-of-file check in REPL ignored LOAD channel, leading to
  heisenbugs.

##### Added

- ASSOC as built-in.

#### Macros

- \*MACROS\* is now an associative list, and macro functions
  are not defined any longer, so their names may clash with
  procedure definitions.

#### Environment

##### Changed

- File suffix for Lisp code is not ".lsp" instead of ".lisp".

##### Added

- IDE: Load editor, save image, and start it.


## [v0.0.17] - 2024-09-12

### Lisp

#### Interpreter

##### Added

- OUT: Output buffer for chars and char lists.
- Compile-time option MICROSCOPIC to strip off almost all
  optional features.
- Type check: failed object in debugger's program info.

##### Fixed

- TIME for C128, C64 and VIC-20.
- Simplified internal I/O channel handling that separates
  program and REPL channels.
- PRINT prints empty strings ("") AKA anonymous symbols.
- Garbage collector doesn't relocate symbol list links.

##### Changed

- Disabled images for the C16 and VIC-20.

#### Environment

##### Fixed

- AUTOLOAD recognizes functions passed by argument.
- Cleaned up to boot on the VIC-20 again.

##### Changed

- MACRO does not define functions any more.
- EDIT-LINE, split out from the edior.
- Queue functions are in separate file for AUTOLOAD.


## [v0.0.16] - 2024-09-06

AUTOLOAD and EDIT.

### Lisp

#### Debugger

##### Added

- ONERROR handler can delegate errors to the debugger to
  be handled as usual, by returning symbol %FAIL.

##### Changed

- Set breakpoints on any kind of object with a name, not
  only user-defined functions.

#### Environment

##### Added

- AUTOLOAD: Loads missing functions and macros, but not
  functions passed as arguments.
- EDIT: Simple text editor.
- COUNT-IF to count by predicate.
- TUNIX terminal control functions ("con.lisp").

##### Fixed

- WITH-GLOBAL: Return value of body.
- SOURCE

#### Interpreter

##### Fixed

- SYMBOL-VALUE of NIL.
- SYMBOL-NAME of NIL.

##### Changed

- No verbose LOAD, VAR and FN.  Would generate unwanted
  output during AUTOLOAD and the like.
- Most built-in functions are opt-out.

##### Added

- NCONC: native implementation to choose instead of the
  built-in version.
- READ-LINE: Reads line as a symbol.
- WITH-IN, WITH-OUT: Redirect channel for body.

### libsimpleio-cbm

- No "direct" mode when writing to non-standard I/O.


## [v0.0.15] - 2024-09-02

### libsimpleio

#### Added

- Terminal control codes to get the cursor position.
- putbackc() to put back another char than the last one.
  Used internally to return cursor positions so far.
- Direct mode with no scrolling and separate CR/LF.

### Interpreter

#### Changed

- TIME supports TARGET\_PET.
- SUBSEQ: Optional to be built-in.
- LENGTH: No not work on (symbol) names any more.
- OUT: Applies limit, set with OUTLIM, to printing object
  names as well.

#### Added

- Optionally built-in APPEND.
- SLENGTH returns the name length of a named object.
  (symbol, special form, built-in)
- Compile-time options NO\_APPEND, NO\_NCONC, NO\_SUBSEQ.


## [v0.0.14] - 2024-08-31

Optimizations to make the upcoming editor usable.  Also, a
simple terminal emulation has been added, which should do
for Unices and CBMs.  The latter via cc65's conio functions.

The C128 and Plus/4 are the only CBMs that work at the
moment.

### libsimpleio

#### Added

- Terminal control codes (see manual section)
 - clear screen
 - position cursor
 - clear/set flags (cursor visibility, reverse mode)

### TUNIX Lisp

#### Interpreter

##### Fixed

- Revived lost tests of APPEND.

##### Added

- NCONC: Destructively concatenates lists.
- OUTLIM limits number of printed character values.

##### Changed

- Now built in (for endurable performance):
 - NTHCDR
 - SUBSEQ
- LENGTH also returns the length of symbol names or names
  of built-ins.

#### Environment

##### Added

- CUT-AT: Destructively splits a list at position.


## [v0.0.13] - 2024-08-29

### Interpeter

#### Added

- Error on lost RETURN or GO.
- Function bodies treat RETURNs by passing them on instantly.

### Environment

- WITH-* macros do not use BLOCK any more.


## [v0.0.12] - 2024-08-28

### General

#### Added

- Manual section on how to add new targets.
- New targets (**untested**):
 - Apple II (TARGET\_APPLE2)
 - Apple II enhanced (TARGET\_APPLE2ENH)
- New targets (**not working**):
 - Atari XL (TARGET\_ATARIXL) - Need adjusted ld65 config.

### TUNIX Lisp

#### Interpreter

##### Changed

- CONIN returns NIL instead of 0 if there's no input,
  so the heap won't get filled with 0's.
- ERROR prints its arguments like OUT, prefixed by
  "ERROR: ".

#### Environment

##### Fixed

- Revived +V+.

##### Added

- +VB+ contains the Git branch name for use with conditional
  code in "user-*-image.lisp".


## [v0.0.11] - 2024-08-27

### TUNIX Lisp

#### Interpreter

##### Changed

- Compile-time option TEST makes all environment tests load.
- OUT: Take any number of arguments and traverse lists.

#### Environment

##### Added

- File 'user-pre-image.lisp' and 'user-post-image.lisp' are
  loaded before and after saving the default image.
  'user-post-image.lisp' is also loaded at image start.
- Macro != (file "alet.lisp").
- Macros !++ and !--: destructive versions of ++ and --.

##### Changed

- SUBSEQ can take negative positions.
- WITH-QUEUE handles only one queue and returns its list.


## [v0.0.10] - 2024-08-26

You'll want the stack checks, young Jedi!

### TUNIX Lisp

#### Interpreter

##### Added

- Do object and stack overflow checks at least once per
  expression evaluation.  Did that with every PUSH/POP in
  development versions.

##### Changed
- Reduced object stack consumption per evaluation (1
  object).

##### Fixed

- Compile-time option NO\_DEBUGGER also excludes code for
  highlighting.

#### Environment

##### Added

- COMPRESS-TREE finds and replaces duplicates subtrees
  across \*UNIVERSE\* symbol definitions.  (See manual.)


## [v0.0.9] - 2024-08-25

Lost pointer fix makes it worth this release.

### libdirectory

#### Changed

- Was 'libdirectory-list'.

#### Added

- Basic I/O functions to open, read and close the current
  directory.

### TUNIX Lisp

#### Interpreter

##### Fixed

- Lost pointer is evaluation of rest arguments.  Happened
  during GC stress test.

##### Added

- EXPERIMENTAL!: CBMs only: OPENDIR, READDIR and CLOSEDIR.

#### Debugger

##### Added

- Short command 'q' to exit the running program and return
  to the top-level REPL.

#### Environment

##### Changed

- PROGN does not use BLOCK.  That caught RETURNs
  unintentionally.

##### Added

- Macro AWHILE: Anaphoric equivalent to WHILE.
- CBMs only: LS to list the current directory.
- Macro WITH-PROF to time stop (aka "to profile")
  expressions.


## [v0.0.8] - 2024-08-24

User experience has been improved so dramatically that
delaying a release wouldn't be acceptable really.

### TUNIX Lisp

#### Environment

##### Changed

- Moved tests into own files.  No test is loaded in
  releases any more.  If compile-time option TEST was
  set, all tests are run as usual (plus a few internal
  interpreter tests at program start.
  Global variable +T?+ tells if TEST was set.
- Initial load time for saving the first image is down
  to less than a sixth of what it was before.
- Prerequisites aren't loaded on demand.  That can be
  made automatic.

#### Interpreter

- On 6502-CPU platforms, only high bytes of pointers are
  checked to tell if they are NIL or not.  Except for NIL
  there are never any objects on the zeropage.  NIL isn't
  either but it could be done to reduce code size a bit or
  two.


## [v0.0.7] - 2024-08-24

Mostly for fixing missing file accident and cleaned up
build scripts.

### Build

- Thorough clean-up of Makefiles.
- Complain if there are foureign files before doing a
  release.

### libsimpleio-cbm

- Print '\' instead of British Pound sign.

### TUNIX Lisp

#### Interpreter

- Increased object stack size to 1K for Comodore C128, C64
  and Plus/4.

#### Environment

##### Fixed

- Lisp environment files for POSITION and SPLIT were missing.


## [v0.0.6] - 2024-08-24

### TUNIX Lisp

#### Environment

##### Fixed

- Macro !? reimplemented and tests added.

##### Added

- Macro CASE: Evaluate conditionally by matching value.
- POSITION: Find position of object in list.
- SPLIT: Split list where object occurs, removing that object.

#### Debugger

##### Fixed

- GC/tag stack over-/underflow checks.
- Show faulty value on type error.
- Tell to which built-in arguments are missing.

##### Added

- New error code ERROR\_NO\_BLOCK\_NAME.

#### Interpreter

##### Fixed

- Global list start/last pointers weren't cleared on program
  start, which is bad with zeropage locations (6502-CPU).

##### Added

- Compile-time option GC\_DIAGNOSTICS to detect zeropage
  issues (undefined globals).


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

>>>>>>> development

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
