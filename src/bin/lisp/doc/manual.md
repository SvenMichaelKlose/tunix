---
title: "TUNIX Lisp"
subtitle: "The Garbage-Collected Manual"
author: "Sven Michael Klose"
lang: "en"
titlepage: true
titlepage-color: "389fff"
titlepage-text-color: "ffffff"
toc: true
footnodes-pretty: true
book: true
...

# Overview

TUNIX Lisp is a highly efficient and scalable Lisp interpreter, written in
ANSI-C.  It is designed for constrained environments, such as classic home
computers (including 6502-based systems), microcontrollers and embedded
systems, but also has its place on modern machines.

Features:

* Interactive REPL.
* Debugger with stepping and breakpoints.
* User-defined error handlers.
* Lean I/O interface.[^io]
* Compacting mark-and-sweep garbage collector.[^gc]
* Supplementary compressed stack.[^stack]
* List compression.
* Saving and loading system images.
* Unified symbol and string handling with data compression features.
* UNDER CONSTRUCTION: Integrated text editor.

Most features can be left out during compilation to make TUNIX Lisp fit
even the most contrained environments.

[^gc]: Planned to be made interruptible to some degree, optionally truly
    interruptible providing a copying garbage collector.
[^io]: Instead of providing a file number for each I/O operation an input
    and/or output channel must be selected beforehand, bridging the gap
    between plain standard I/O and multi-stream handling without making the
    API more complex from the start, supporting operation in maximally
    constrained environments.
[^stack]: It holds byte-sized tags instead of larger return addresses.
    Also to support architectures with limited CPU stacks, like those with
    MOS-6502 CPUs.

This distribution builds executables for these platforms using the cc65 C
compiler suite:

* Commodore C128
* Commodore C16
* Commodore C64
* Commodore Plus4
* Commodore VIC-20 (+27K)

It also compiles on regular Unixoids, using the GNU compiler toolchain or
compatibles.

## Some differences to other dialects

Here is how most other Lisps translate to TUNIX Lisp:

| Most other dialects    | TUNIX Lisp      |
|------------------------|-----------------|
| backquote sign '`'     | dollar sign '$' |
| (SETQ c v)             | (= c v)         |
| (RPLACA c v)           | (SETCAR c v)    |
| (RPLACD c v)           | (SETCDR c v)    |
| (MAKE-SYMBOL x)        | (SYMBOL l)      |
| (SYMBOL-VALUE s)       | (VALUE s)       |
| (FILTER f l)           | (@ f l)         |
| (LAMBDA (args . body)) | (args . body)   |
| #\\A                   | \\A             |
| (cond +l)              | (? +l)          |
| (= num1 num2)          | (== num1 num2)  |

Because the backquote (`) is not part of the charsets of old machines
TUNIX Lisp intends to support, the dollar sign ($) is used as the
abbreviation for QUASIQUOTE.

MEMBER and FIND are comparing with EQ instead of EQL as these functions
are used internally as well and need to be fast.  Use NEMBER-IF or FIND-IF
together with EQL to match numbers by value.

LAMBDA is not around yet.  Function expressions are quoted when used as
arguments to other functions.  That makes compiling them to 'native'
function impossible, so something similar will have to be in later
versions.

### Symbols are strings

Symbols have a case-sensitive name and a value and they also serve as
strings.  They can be converted to and from character value lists:

~~~lisp
(symbol '(\A \B \C)) -> "ABC"
~~~

Symbols may also be anonymous, with no name at all.  Calling SYMBOL with
no arguments creates a unique and anonymous symbol that won't get reused
when SYMBOL is called without arguments again.  As this is true for all
symbols with no name, an empty string ("") can be used as well.

~~~lisp
(eq (symbol) (symbol))  ; -> NIL
(eq "" (symbol))        ; -> NIL
(eq "" "")              ; -> NIL
~~~

SYMBOL will issue an error if it is passed a dotted pair.

### Built-in = instead of SETQ

### Macro ?: A more compact version of COND

Macro ? is used instead of COND.  It does not require each
condition/consequence pair to be a list.  Any remaining expression with
no following one to make a pair is the default, so no T condition is
required:

~~~lisp
; NOT used in TUNIX Lisp.
(cond
  ((hungry?) (go 'shop))
  (t (have-fun)))
(cond
  ((hungry?)  (go 'shop))
  ((thirsty?) (go 'fridge))
  (t (have-fun)))

; TUNIX Lisp version.
(? (hungry?)
   (go 'shop)
   (have-fun))
(?
  (hungry?)  (go 'shop)
  (thirsty?) (go 'fridge)
  (have-fun))
~~~

## Memory consumption

### Heap

Object allocation is fast, requiring bumping up the pointer to the top of
the growing heap, and a boundary check to trigger garbage collection when
the heap is full.

| Data type              | heap  |
|------------------------|-------|
| cons                   |   5   |
| number (32 bit signed) |   5   |
| symbol (also string)   | 4-260 |

### CPU stack, object stack, and tag stack

Alongside the CPU stack a separate garbage-collected object stack holds
function arguments and objects that need to be relocated during garbage
collection.  An additional raw stack holds return tags of byte size
instead of full return addresses, and raw pointers to built-in procedure's
argument definitions.

### Inevitable creation of list elements

APPLY copies all arguments but the last one.

# Installation

"Installing" a binary release is the easiest way to go exploring.  I'd
rather recommend compiling it yourself.

## Getting a binary release

Download the latest binary from
[https://github.com/SvenMichaelKlose/tunix/releases](https://github.com/SvenMichaelKlose/tunix/releases).

The name of the ZIP file contains the project's name "tunix", followed by
its release version, ID in the public Git repository (short SHA hash), and
finally the release date, followed by the opligatory ZIP suffix.

It should look like this:

~~~
tunix.v0.0.5+bca5411.2024-08-22.zip
       ^^^^^ ^^^^^^^ ^^^^^^^^^^
         |      |        |
         |      |   release date
         |   Git SHA
      version
~~~

*** TODO: See also "Version information". ***

The version, "0.0.5" in this case, contains a major, minor and patch
version according to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).
TUNIX Lisp is a bit different as it has a major version number of "0",
indicating that it's not meant for production where you expect things to
not change from one day to the other.  To make it even worse, the minor
version also being "0" means that absolutely everything could change, no
matter if there'll be hell or high water.  It's about making TUNIX grown
up enough to be able to follow the Semantic Versioning rules in the first
place.  But we're trying to keep the pain away.  The patch level increases
with every release - that happens if a bunch of changes happened that
makes everyones life easier.  You must download the latest.  The others
are kept for the protocol only.

Things you must expect to change sooner than later:

* Required special keyword (like the notoriuos LAMBDA) to tell function
  expressiod from regular expressions.  It'll be required to make the
  language more comfortable when it comes to lexical scope, and to have a
  compiler produce effective code.

## Unpacking

Download the latest release and unpack it.  On a unixoid command-line
(Linux/Mac) this should do:

~~~sh
unzip tunix.<LatestVersion>.zip
~~~

It contains directory "tunix" and subdirectories for all supported
platforms, e.g. "tunix/c64".  You can step into one of those and run TUNIX
Lisp in your favourite emulator, or you can also transfer the files to
your platform, depending on what it supports.  For Commdore 8-bit machines
there are SD card readers, also known as SD2IEC drives, available.

## Running on Linux/Mac/BSD, etc.

If you're running a something Unixoid, step into directory "tunix/unix"
and shoot it up by typing "./lisp":

~~~sh
cd tunix/unix
./lisp
~~~

You have to step into "tunix/unix" or TUNIX Lisp won't find the other
files it needs to get going.

## Installing VICE and YAPE mmulators

The "VersatIle Commodore Emulator" is the most popular one for Commodore
8-bit machines.  Commodore C16 and Plus/4 fanatics will insist on using
YAPE as it's more compatible to the original.

### The VersatIle Commdore Emulator (VICE)

#### **Linux**

For most Linux distributions, VICE can be installed directly from the package manager.

- **Debian/Ubuntu-based distributions**:
  ```sh
  sudo apt-get update
  sudo apt-get install vice
  ```

- **Fedora**:
  ```sh
  sudo dnf install vice
  ```

- **Arch Linux**:
  ```sh
  sudo pacman -S vice
  ```

If VICE is not available in your distribution's repositories, you may need
to compile it from source. Visit the
[VICE website](https://vice-emu.sourceforge.io/) for more information.

#### **macOS**

VICE can be installed via Homebrew on macOS:

- **Using Homebrew**:
  ```sh
  brew install vice
  ```

Alternatively, you can download the latest macOS binary from the
[VICE website](https://vice-emu.sourceforge.io/) and follow the
instructions provided there.

#### **Windows**

For Windows, you can download the latest VICE binary from
the [VICE website](https://vice-emu.sourceforge.io/).
After downloading, extract the archive to a directory of your choice and
run the appropriate executable (e.g., `x64.exe` for C64 emulation).

### Yet Another Plus/4 Emulator (YAPE)

#### **Windows**

YAPE is primarily a Windows-based emulator.  You can download it from the
[YAPE website](http://yape.homeserver.hu/).  After downloading, extract
the archive and run `yape.exe`.

#### **Linux and macOS**

YAPE is not natively available for Linux or macOS, but you can run it
using Wine, a compatibility layer for running Windows applications on
Unix-like operating systems.

- **Install Wine**:
  - **Debian/Ubuntu**:
    ```bash
    sudo apt-get install wine
    ```
  - **Fedora**:
    ```bash
    sudo dnf install wine
    ```
  - **macOS** (using Homebrew):
    ```bash
    brew install --cask wine-stable
    ```

- **Run YAPE with Wine**:
  After installing Wine, download YAPE from the
  [YAPE website](http://yape.homeserver.hu/) and run it
  with:
  ```bash
  wine yape.exe
  ```

### Additional Resources

For more detailed installation instructions or troubleshooting, please
refer to the respective emulator's website:

* [VICE Emulator](https://vice-emu.sourceforge.io/)
* [YAPE Emulator](http://yape.homeserver.hu/)

*** TODO: Apple IIe emulator installation ***

## Building TUNIX from source

Building TUNIX from source code is highly recommended if you want to stay
up to date, especially for getting patches that remove bugs - naturally
these occur often in early software.

### General instructions for all platforms

#### Cloning the Repository

To begin, clone the TUNIX repository:

```bash
git clone https://github.com/svenklose/tunix.git
cd tunix
```

#### Fetching third-party code

After cloning the repository, you must fetch all the required third-party
code and build that first:

```bash
git submodule update --init --recursive
make host
```

#### Building all targets

To build binaries for all supported targets, run:

```bash
make allworlds
```

#### Building for a specific target

If you want to build TUNIX for a specific platform, use the following command:

```bash
make world TARGET=<target>
```

Replace `<target>` with one of the supported targets listed below.

#### Supported targets:

| Target   | Description                   |
|----------|-------------------------------|
| `c128`   | Commodore C128                |
| `c16`    | Commodore C16                 |
| `c64`    | Commodore C64                 |
| `pet`    | Commodore PET (doesn't start) |
| `plus4`  | Commodore Plus/4              |
| `sim6502`| cc65's sim65                  |
| `unix`   | GCC-compatible toolchain      |
| `vic20`  | Commodore VIC-20              |

### Linux

#### Prerequisites

Ensure you have the necessary tools installed:

```bash
sudo apt-get update
sudo apt-get install git build-essential
```

#### Building

```bash
git submodule update --init --recursive
make host
make allworlds
```

### macOS

#### Prerequisites

Install the necessary tools using Homebrew:

```bash
brew install git
```

#### Building

```bash
git submodule update --init --recursive
make host
make allworlds
```

### Windows

#### Prerequisites

Set up a Unix-like environment using MSYS2 or MinGW:

1. **Install MSYS2** from [msys2.org](https://www.msys2.org/).

2. **Update the Package Database**:
   ```bash
   pacman -Syu
   ```

3. **Install the Development Tools**:
   ```bash
   pacman -S base-devel git mingw-w64-x86_64-gcc
   ```

#### Building

1. **Clone the Repository and Fetch Third-Party Code**:
   ```bash
   git clone https://github.com/svenklose/tunix.git
   cd tunix
   git submodule update --init --recursive
   make host
   ```

2. **Build TUNIX**:
   ```bash
   make allworlds
   ```

   Or for a specific target like Unix:
   ```bash
   make world TARGET=unix
   ```

# Using TUNIX Lisp: REPL, autoloader and debugger

When firing up the interpreter the first time, it loads the most essential
code required to load more code on demand, and creates a boot image which
is loaded instead on next program start.  You then end up in the REPL
(read-eval-print-loop).  It reads an expression, evaluates it, and prints
the result.  Then it starts over if the input channel hasn't been closed.

~~~lisp
(dotimes (i 10) (print i))
0 1 2 3 4 5 6 7 8 9
~~~

If function is missing, the error handler AUTOLOAD tries to load its
source file by appending the ".lsp" suffix to its name.  This also works
with macros.

# Definiton of permanent symbols

Symbols that are meant to remain untouched by the garbage collector must
be added to the global \*UNVIERSE\* list.  Built-in pecial forms FN and
VAR, which define functions and global variables, do that automatically.

~~~lisp
; Define permanent function.
(fn welcome ()
  (out '"Hello World!")
  (terpri))

; Define permanent variable.
(var x nil)
~~~

# Functions

Functions are lists starting with an argument definition followed by a
list of expressions.  The result of the last expression is returned.

The LAMBDA keyword is not around at the moment but it has to be to make
the compiler work.

~~~lisp
; Function with no arguments, returning symbol NIL.
(nil)

; Function with no arguments, returning number '3'.
(nil
  1
  2
  3)

; Function returning its argument.
((x)
  x)

; Functions returning their argument list.
(x
  x)
((first . rest)
  (cons first rest))
~~~

Anonymous functions can be used as the first element of an expression
without quoting:

~~~lisp
; Print number '100'.
(((x)
   (print (+ 1 x)))
 99)
~~~

Anonymous functions as arguments need to be quoted though:

~~~lisp
; Add 1 to each number in list.
(@ '((n) (++ n)) '(l 2 3))
~~~

The QUASIQUOTE (short form "$") can be used to emulate read-only lexical
scope by unquoting outer values:

~~~lisp
; Make a function that adds X to its argument.
(fn make-adder (x)
  $((a)
     (+ a ,x)))
~~~

## Rest arguments

If an argument definition ends with a dotted pair, the last argument will
contain the rest of the arguments passed to the function as a list or NIL.

(fn cool-exmaple-missing (first . rest))
(fn cool-exmaple-missing (first second . rest))

## Optional arguments

Rest arguments can be used to implement one or more optional arguments
with defaults.

~~~lisp
(fn subeq (first . optional)
  (= optional (or (car optional) 0)))
~~~

~~~lisp
(fn subeq (first . optionals)
  (with ((optional1 (or (car optionals) 0))
         (optional2 (or (cadr optionals) 0)))
    (print optional1)
    (print optional2)
    (terpri)))
~~~

In this example, if optional is NIL, CAR will also return NIL, making the
OR-expression return number 0.  This scheme can also be applied to
multiple arguments:

## Argument type descriptions in this manual

Built-in functions have character-based and typed argument definitions.
They are also used, padded with spaces, to describe arguments in this
manual for all procedures (functions, macros and special forms).

| Code | Type                                    |
|------|-----------------------------------------|
|  x   | anything                                |
|  c   | cons                                    |
|  l   | list (cons or NIL)                      |
|  n   | number                                  |
|  s   | symbol                                  |
|  a   | memory address (positive number)        |
|  b   | byte value                              |

They may also have prefixes:

| Prefix | Description           |
|--------|-----------------------|
|   +X   | any number of type X  |
|   ?X   | optional              |
|   'X   | unevaluated           |

# Input/output

TUNIX Lisp boils I/O down to its basics: one channel for input and one for
output, initially wired to "standard I/O", like your terminal with screen
and keyboard.  Input and output can each be switched to other channels.
If you launch a LOAD command to execute a Lisp file, the input channel is
connected to that file until it's been read entirely, but in general a
channel can be directed to another one anytime.

## READing and PRINTing expressions

Expressions can be read and written using built-in functions READ and
PRINT.  Strings and chars have dedicated formats:

| Type format examples | Description                     |
|----------------------|---------------------------------|
| (a . d)              | "dotted pair" (must be quoted), |
| "string"             | String.  Escape is "\\".        |
| \\A                  | Character value.                |

READ and PRINT also support abbreviations if compiled in:

| Expression         | Abbreviation |
|--------------------|--------------|
| (quote x)          | 'x           |
| (quasiquote x)     | $x           |
| (unquote x)        | ,x           |
| (unquote-splice x) | ,@x          |

## Catching I/O errors and state

## Character-based I/O

## Input and output channel

An input and an output channel can be switched between open streams
separately using functions SETIN and SETOUT.  Symbols STDIN and STDOUT
contain the standard I/O channel numbers.

~~~lisp
; Switch to standard I/O channels.
(setin stdin)
(setout stdout)
~~~

The currently active channels numbers are in symbols FNIN and FNOUT.

New channels are created by OPEN to access files:

~~~lisp
(fn user-defined-load (pathname)
  (with ((last-result  nil)
         (old-channel  fnin)
         (new-channel  (open pathname)))
    (unless (err)
      (setin new-channel)
      (while (not (eof))
        (= last-result (eval (read))))
      (setin old-channel)
      last-result)))
~~~

## Terminal control codes

| Code     | Function           |
|----------|--------------------|
|   0      | Do nothing.        |
|   1 x y  | Position cursor.   |
|   2 f    | Clear flags.       |
|   3 f    | Set flags.         |
|   4      | Get flags.         |
|   5      | Get X position.    |
|   6      | Get Y position.    |
|  10      | Line feed.         |
|  12      | Clear screen.      |
|  13      | Carriage return.   |

| Flag | Function             |
|------|----------------------|
|   1  | Cursor visibility    |
|   2  | Reverse mode         |
|   4  | Direct mode          |

Flags may be combined.

# Error handling and debugging

The debugger is invoked in case of an error unless ONERROR has been
defined.  Beatiful things can be done by handling errors automatically,
but let's get our hands on the debugger first.

## The debugger REPL

| Variable | Description                            |
|----------|----------------------------------------|
|   *b*    | List of symbols that are breakpointed. |
|   *r*    | Initial return value of current REPL.  |

The debugger is the REPL in debug mode.  It prints a status info before
waiting for user input, so you know where the program execution has been
interrupted.  It has this format:

~~~
Debugger <number of nested debuggers>:
Error #5: <reason for break>
Rvalue: <last expression's (and debugger's) return value>
In:
<top-level expression with current one highlighted>
~~~

The debugger takes expressions like the regular REPL, plus some commands
consisting of a single character to step through the code conveniently.
If another error occurs, yet another debugger REPL will be invoked and the
"number of nested debuggers" incremented.

The current expression is either the one that failed, or the one that will
be evaluated next in cause the debugger stopped at a breakpoint (and no
error number and description is shown).

The return value of the debugger will change with every expression you
enter, except when using aforementioned short commands.  In case of an
error, that's the value you want to replace with a valid one before
continuing program execution.  Symbol \*R\* contains the return value when
the debugger was invoked, should you want to see or use it again although
you've replaced it already – just enter "*r*" and it'll be restored.

These are the available short commands:

| Command | Description                                    |
|---------|------------------------------------------------|
| c       | Continue program execution.                    |
| k       | Ignore expression.                             |
| q       | Exit REPL.                                     |
| x       | Exit program.                                  |
| s       | Step into user-defined procedure.              |
| n       | Execute current expression in whole.           |
| pX      | Evaluate and print expression X.  (No macros!) |
| bS      | Set breakpoint on procedure S.                 |
| b       | Print breakpoints.                             |
| dS      | Delete breakpoint on procedure S.              |
| d       | Delete all breakpoints.                        |

Command "p" evaluates the expression immediately following it.  A macro
expansion is *not* performed and it'll *not* change the debugger's return
value.

## Stepping through the code

Short command 's' will step to the next argument of the current
expression, evaluation what's on the way or enter the currently
highlighted function if all arguments have been dealt with.  With 'n' the
function and all its arguments are evaluated, taking you to the next
expression in the list.  If you had it the program, you can exit it with
short command 'q' and take a break yourself.

IDEA:
* step into newly entered expression
* step into restarted expression.  Already changed values are a problem
  then.

## Breakpoints

Global variable \*B\* is a list procedures' names which, if called, will
invoke the debugger.

You can modify \*B\* using the regular set of procedures:

~~~lisp
; Set breakpoint on procedure SUBSEQ.
(push 'subseq *b*)

; Delete a breakpoint.
(= *b* (remove 'subseq *b*))

; Delete all breakpoints.
(= *b* nil)
~~~

Inside the debugger REPL that's inconvenient as every regular expression
changes the debugger's return value.  Use short commands 'b' and 'd'
instead.

~~~lisp
bsubseq ; Set breakpoint on SUBSEQ.
dsubseq ; Delete breakpoint on SUBSEQ.
d       ; Delete all breakpoints.
~~~

## User-defined error handler ONERROR

| Function        | Description                           |
|-----------------|---------------------------------------|
| (onerror n x x) | User-defined error handler.           |
| (ignore)        | Break and continue with LOAD or REPL. |

If defined, user-defined function ONERROR is called on errors, except for
internal ones that need to halt the interpreter to avoid unexpected
behaviour and thus damage.  Errors happening inside ONERROR will cause it
to be called again.

ONERROR is called with the error code, the current REPL (top-level)
expression, and the faulty expression within it:

The handler must return an alternative value for the failed expression.
If that expression can be evaluated again, you perhaps should not forget
to macro-expand it beforehand.

To delegate error handling to the debugger as usual, the handler has to
return symbol %FAIL.

~~~lisp
(fn onerror (errcode repl faulty)
  (out "ONERROR handler called!")(terpri)
  ; We don't handle errors so unleash the debugger on it.
  '%fail)
~~~

The current REPL's top-level expression can be ignored by calling IGNORE
instead.

### Error codes

These are the error codes found in the CODE argument and in the debugger's
heading printed when invoked:

| ID (ERROR_...)  | Code | Description                     |
|-----------------|------|---------------------------------|
| TYPE            | 1    | Unexpected object type.         |
| ARG\_MISSING    | 2    | One or more missing arguments.  |
| TAG\_MISSING    | 3    | BLOCK tag couldn't be found.    |
| TOO\_MANY\_ARGS | 4    | Too many arguments.             |
| NOT\_FUNCTION   | 5    | Object is not a function.       |
| ARGNAME\_TYPE   | 6    | Argument name is not a symbol.  |
| NO\_BLOCK\_NAME | 7    | BLOCK name is missing.          |
| OUT\_OF\_HEAP   | 8    | Out of heap.                    |
| NO\_PAREN       | 9    | ')' missing.                    |
| STALE\_PAREN    | 10   | Unexpected ')'.                 |
| SYM\_TOO\_LONG  | 11   | Symbol longer than MAX\_SYMBOL. |
| QUOTE\_MISSING  | 12   | '"' missing.                    |
| LOST\_RETURN    | 13   | RETURN name didn't match BLOCK. |
| LOST\_GO        | 14   | GO outside BLOCK.               |
| NEGATIVE        | 15   | Positive number expected.       |
| FILEMODE        | 16   | Illegal mode for OPEN.          |
| USER            | 17   | ERROR function was called.      |
| INTERNAL        | 18   | Returned to operating system.   |

#### ERROR\_OUT\_OF\_HEAP

Returns to the current REPL and does a garbage collection before calling
an ONERROR handler or debugger.

Compile-time option ONETIME\_HEAP\_MARGIN specified the number of heap
bytes that are kept for calling an ONERROR handler.

# Built-in functions

## General

| Function   | Description                            |
|------------|----------------------------------------|
| (exit ?n)  | Exit program or interpreter with code. |

### (exit ?n): Exit program or interpreter with exit code.

When called without arguments the program is stopped and control is
returned to the top-level REPL.  When called with a number that number is
the exit code for the interpreter which will terminate immediately.

## Heap

| Function   | Description                            |
|------------|----------------------------------------|
| (gc)       | Free unused objects.                   |
| (free)     | Number of free bytes on heap.          |

### (gc): Free unused objects.

Triggers the garbage collector.  It marks all objects linked to variable
\*UNIVERSE\*, compacts the heap and relocates all pointers.

### (free): Number of free bytes on heap.

Returns the maximum number of bytes that could be allocated.  That number
is likely to be less but can amount to the size of a symbol with the
biggest possible name length.

## Definitions

| Special form                  | Type                      |
|------------------------------|---------------------------|
| (var 'name x)                | Define symbol with value. |
| (fn 'name 'args '+body)      | Define function.          |
| (special 'name 'args '+body) | Define special form.      |

| Function   | Description                              |
|------------|------------------------------------------|
| (source s) | Return defining expression for a symbol. |

| Variable | Description                                     |
|----------|-------------------------------------------------|
|  \*v?\*  | Verbosity of FN, LOAD, SPECIAL and VAR.         |
| \*alv?\* | AUTOLOAD erbosity of FN, LOAD, SPECIAL and VAR. |

### Verbosity (\*V?\*, \*ALV?\*)

By default \*V\*? is set to T, causing FN, LOAD, SPECIAL and VAR
to print messages when they are in action.  That's impractical
when auto-loading procedures on demand as the message are likely
to spoil the screen.  \*ALV?\* is T by default, to make it easier
to track down errors.

### (fn 'name 'args '+body): Define permanent, named function.

Adds NAME to \*UNIVERSE\* and assigns an function expression of
the form "(args . body)".

### (special 'name 'args '+body): Make special form.

Special forms are functions that receive their arguments unevaluated,
so the caller doesn't have to QUOTE them.  MACRO and QUASIQUOTE are
such user-defined special forms.

### (var 'name init): Define permanent, named variable.

Adds NAME to \*UNIVERSE\* and assigns it evaluated INIT.

### (source s): Return defining expression for a symbol.

## Evaluation and flow control

| Function       | Description                            |
|----------------|----------------------------------------|
| (quote 'x)     | Return argument unevaluated.           |
| (apply f +x)   | Call function with list of arguments.  |
| (funcall f +x) | Call function with explicit arguments. |
| (eval x)       | Evaluate expression.                   |
| (? cond +x)    | Evaluate expression conditionally.     |
| (and +x)       | Logical AND.  Evaluate until NIL.      |
| (or +x)        | Logical OR.  Evaluate until not NIL.   |
| (block 's +x)  | Named block with expression list.      |
| (return x ?'s) | Return from named block with value.    |
| (go 's)        | Jump to tag in named block.            |

### (quote x)

Returns argument unevaluated.  Suppresses replacing symbols by their
values on evaluation.

~~~lisp
; Define variable X, containing the string "What a day!".
(var x "What a day!")
x         -> "What a day!"
(quote x) -> x
'x        -> x  ; Short form.
~~~

### (apply fun . args): Apply function.

Calls function FUN.  Unlike the rather straightforward FUNCALL, which
takes its arguments as provided, APPLY expects the last element of ARGS to
be a list, which is then appended to the previous elements:

~~~lisp
(fn list x
  x)

(apply list '(10 11))   -> (10 11)
(apply list 1 2 '(3 4)) -> (1 2 3 4)
~~~

The reason for this is that it takes away the need to do that kind of
concatenation oneself repeatedly, which would happen a lot otherwise.

### (funcall f +x): Call function.

Basically calls function F with the list of arguments X.

~~~lisp
; Basically the same:
(funcall 'print "Hello world!")
(print "Hello world!")

(funcall (?
           (eq color 'red)    print-red
           (eq color 'yellow) print-yellow
           (eq color 'green)  print-green)
         "Hello world!")
~~~

### (eval x): Evaluate expression.

Evaluates expression X and it's subexpressions.

### (? x +x): Conditional evaluation

Returns the second argument if the first one evaluates to non-NIL.
Otherwise the process is repeated starting with the third argument, unless
there is only one argument left which is then the default.

~~~lisp
(? nil
   1)  ; -> nil
(? nil
   1
   2)  ; -> 2
(? nil
   1
   2
   3)  ; -> 3
(? t
   1
   2)  ; -> 1
(? t)  ; -> nil
~~~

### (and +x)

Evaluates all arguments in order unless one evaluates to NIL.  The value
of the last evaluation is returned.

~~~lisp
(and 1 2 nil) ; -> nil
(and 1 2)     ; -> 2
~~~

AND will issue an error if it is passed a dotted pair.

### (or +x)

Evaluates all arguments unless one evaluates to non-NIL.  The value of the
last evaluation is returned.

~~~lisp
(or 1 nil) ; -> 1
(or nil 2) ; -> 2
~~~

OR will issue an error if it is passed a dotted pair.

### (block name . body), (return x block-name), (go tag)

Evaluates the list of expressions in BODY, returning the value of the last
unless a RETURN from the block has been initiated.  The name of the block
passed to RETURN has to match.  It is NIL, if not specified.

~~~lisp
(block foo
  'a
  (return 'b foo)
  'c)   ; -> b
~~~

Blocks of name NIL are used for loops.  For the purpose of just butting up
expressions use T instead to make RETURNs for name NIL drop through.

~~~lisp
(macro progn body
  $(block t ; We don't want to catch returns.
     ,@body))
~~~

BLOCK also handles jumps initiated by GO.  A jump destination, the "tag",
must be the same symbol passed to GO unquoted.  It is an error if the tag
cannot be found in any of the parent blocks in the current function.  If
no expression follows the tag, NIL is returned.

~~~lisp
; Print "1".
(block nil
  (print 1)
  (go jump-destination)
  (print 2)
  jump-destination) ; -> nil

; Print "1" and "3".
(block nil
  (print 1)
  (go jump-destination)
  (print 2)
  jump-destination
  (print 3))        ; -> 3
~~~

## Equality

| Function    | Description                          |
|-------------|--------------------------------------|
| (eq a b)    | Test if objects are the same.        |
| (eql a b)   | Test if numbers are the equal or EQ. |
| (equal a b) | Test if trees are EQL.               |

### (eq a b): Test if objects are the same.

Tests if two objects are the very same.

Numbers usually are not as they are not looked-up for reuse like symbols.
Use EQL instead.

### (eql a b): Test if numbers are the equal or EQ.

Like EQ except for numbers: their true values are compared using function
== instead.

### (equal a b): Test if trees are EQL.

Like EQL but traversing down conses, allowing to compare lists and trees
(lists of lists).

## Predicates

| Function     | Test on...        |
|--------------|-------------------|
| (not x)      | NIL               |
| (atom x)     | not a cons        |
| (cons? x)    | cons              |
| (symbol? x)  | symbol            |
| (number? x)  | number            |
| (builtin? x) | built-in function |
| (special? x) | special form      |

All predicates except NOT and SYMOL? return their argument instead of T
when true.

TODO: Impressive example where it's advantagous.

### (not x): NIL

Returns T on NIl and NIL otherwise.

### (atom x): not a cons

Returns its argument if it's an atom, except for NIL for which T is
returned.

### (cons? x): cons

Returns its argument if it is a cons, NIL otherwise.

### (symbol? x): symbol

Returns its argument if it is an atom.  T is returned for NIL.  And NIL is
returned for conses.

### (number? x): number

Returns its argument if it is a number or NIL otherwise.

### (builtin? x): built-in function

Returns its argument if it is a built-in or NIL otherwise.

### (special? x): special form

Returns its argument if it is a special form or NIL.

## Symbols

| Function         | Description                           |
|------------------|---------------------------------------|
| (symbol l)       | Make symbol with name from char list. |
| (= 's x)         | Set symbol value.                     |
| (symbol-value s) | Get symbol value.                     |
| (symbol-name s)  | Get name as List of char numbers.     |
| (slength s)      | Get name length.                      |
| (char-at s n)    | Char of symbol name.                  |

### (symbol l): Make symbol with name from char list.

Already existing symbols are reused.

~~~lisp
(symbol '(110 105 108)) ; -> nil
~~~

### (= 's x): Set symbol value.

The symbol argument is not evaluated.  Returns the value.

### (symbol-value s): Get symbol value.

### (symbol-name s): Get name.

Returns the name of a symbol or built-in as a list of numbers being the
character values.

~~~lisp
(symbol-name nil) ; -> (110 105 108)
~~~

### (slength s): Get name length.

Get length of symbol or built-in.

~~~lisp
(slength nil) ; -> 3
~~~

### (char-at s n): Char of symbol name.

~~~lisp
(char-at nil 1) ; -> 105
~~~

## Conses

| Function     | Description                         |
|--------------|-------------------------------------|
| (car l)      | Return first value of cons or NIL.  |
| (cdr l)      | Return second value of cons or NIL. |
| (setcar c x) | Set first value of cons.            |
| (setcdr c x) | Set second value of cons.           |

A 'cons' points to two other objects, called 'car' and 'cdr' for
historical reasons.  They could also be called 'first' and 'second',
'first' and 'rest' or 'head' and 'tail'.  However: they are just two
object pointers packed together to form a pair.  A single cons is written
with a dot in the middle which separates the two objects it contains.
It's called a "dotted pair":

~~~lisp
(obj-a . obj-b)
~~~

### (car l)/(cdr l): Return first or second value of cons.

CAR and CDR expect a list (cons or NIL) and return the first or second
object a cons contains.  If the argument is NIL, CAR and CDR return NIL.

~~~lisp
(car nil)   ; -> nil
(cdr nil)   ; -> nil

(var our-cons '(a . b))
(car our-cons) ; -> a
(cdr our-cons) ; -> b
~~~~

Because lists a conses chained up via their CDRs, this happens with conses
of lists:

~~~lisp
(var our-list '(a b))
(car our-list) ; -> a
(cdr our-list) ; -> (b)
~~~

### (setcar c x)/(setcdr c x): Set first/second value of cons.

Sets the first or second value of a cons.  Passing anything else but a
cons, e.g. NIL, is an error.  The modified cons is returned otherwise.

~~~lisp
(var our-cons '(a . b))
(setcar our-cons 'new)     ; -> (new . b)
(setcdr our-cons 'values)  ; -> (new . value)
(setcdr our-cons nil)      ; -> (new)
~~~

Setting the CDR of a *compressed cons* is also an error.  See section
[compressed conses](#compressed-conses) for details.

## Images

| Function  | Description                                |
|-----------|--------------------------------------------|
| (isave s) | Save heap image.                           |
| (iload s) | Load heap image and start function ISTART. |

Compile-time option NO\_IMAGES must be undefined to use these.  If an
image file called 'image' exists in the current directory, that is loaded
instead of default Lisp files.

## Lists

These functions are around because the interpreter needs them internally.

| Function          | Description                         |
|-------------------|-------------------------------------|
| (length l)        | Return length of list or name.      |
| (@ f l)           | Run elements through function.      |
| (butlast l)       | Copy list but not its last element. |
| (last l)          | Return last cons of list.           |
| (member x l)      | Return list starting with X.        |
| (remove x l)      | Copy list except element X.         |

These are built-in or performance issues surface quickly:

| Function          | Description                         |
|-------------------|-------------------------------------|
| (nconc +l)        | Destructively concatenae lists.     |

### (butlast l): Copy list but not its last element.

~~~lisp
(butlast '(1 2 3)) ; -> (1 2)
~~~

BUT LAST will issue an error if it is passed a dotted pair.

### (last l): Return last cons of list.

Return the last cons of a list, not the object it contains.

~~~lisp
(last '(1 2 3)) ; -> (3)
~~~

LAST will issue an error if it is passed a dotted pair.

### (length l): Return length of list or name.

~~~lisp
(length nil)        ; -> 0
(length '(a b)))    ; -> 2
(length '(a b c)))  ; -> 3
~~~

LENTGH also counts CDRs of dotted pairs:

~~~lisp
(length '(a b . c)))  ; -> 3
~~~

Also, the length of symbol names or names of built-ins is returned:

~~~lisp
(length "TUNIX")    ; -> 5
(length car)        ; -> 3
~~~

### (member x l): Return cons containing X.

~~~lisp
(member 'b '(a b c)) ; -> '(b c)
~~~

MEMBER uses EQL as the predicate, so numbers will also match.

~~~lisp
(member 2 '(1 2 3)) ; -> '(2 3)
~~~

### (nconc +l): Destructively concatenate lists.

NCON is a function that destructively concatenates a series of lists.   It
is more efficient than APPEND, because it avoids the overhead of copying
elements and directly links the lists together.

⚠️ Lists returned by QUOTE will be modified, changing your code!  Use
BACKQUOTE ($) instead, or use COPY-LIST before passing your list to NCONC.

### (remove x l): Copy list except element X.

~~~lisp
(remove 'b '(a b c)) ; '(a c)
~~~

Uses EQ as the predicate, so REMOVE-IF must be used together with EQL to
match numbers.

### (@ f l): Filter list by function

Call function F for each element in l and returns a new list containing
the return values of F.

~~~lisp
(@ ++ '(1 2 3)) ; -> (2 3 4)
~~~

Also handles dotted pairs, filtering the last atom if it is not NIL.  This
is supported because of rest argument definitions (which are dotted
pairs).

~~~lisp
(@ ++ '(1 2 . 3)) ; -> (2 3 . 4)
~~~

## Numbers

### Comparing

| Function | Description           |
|----------|-----------------------|
| (== n n) | equal                 |
| (> n n)  | greater than          |
| (< n n)  | less than             |
| (>= n n) | greater than or equal |
| (<= n n) | less than or equal    |

### Arithmetics

| Function | Description                             |
|----------|-----------------------------------------|
| (+ n n)  | Add numbers.                            |
| (- n n)  | Subtract rest of numbers from first.    |
| (\* n n) | Multiply numbers.                       |
| (/ n n)  | Divide first number by rest of numbers. |
| (% n n)  | Modulo of numbers.                      |

These operators take two arguments instead of variadic
lists.[^arith-future]

[^arith-future]: To be changing in the future.

### Increment/decrement

| Function | Description                             |
|----------|-----------------------------------------|
| (++ n)   | Increment (add 1).                      |
| (-- n)   | Decrement (take 1).                     |

~~~lisp
(var x 23)
(++ x)      ; 24
(-- x)      ; 22
x           ; 23
~~~

### Bit manipulation

| Function      | Description    |
|---------------|----------------|
| (bit-and n n) | AND            |
| (bit-or n n)  | Inclusive OR.  |
| (bit-xor n n) | Exclusive OR.  |
| (bit-neg n)   | Flip all bits. |
| (>> n nbits)  | Shift right.   |
| (<< n nbits)  | Shift left.    |

## I/O

| Function         | Description                          |
|------------------|--------------------------------------|
| (read)           | Read expression.                     |
| (print x)        | Print expression.                    |
| (load name)      | Load and evaluate file.              |
| (require +name)  | Load missing definition of NAME.     |
| (open name mode) | Open file and return channel.        |
| (err)            | Return number of last error or NIL.  |
| (eof)            | Tell if read reached end of file.    |
| (setin n)        | Set input channel.                   |
| (setout n)       | Set output channel.                  |
| (in)             | Read char.                           |
| (read-line)      | Read line as a symbol.               |
| (putback +n)     | Put last char back to input.         |
| (conin)          | Read char from console.              |
| (out x)          | Print char or string, lists of them. |
| (outlim n)       | Limit number of char values printed. |
| (with-in x +l)   | Redirect input channel for body.     |
| (with-out x +l)  | Redirect output channel for body.    |
| (terpri)         | Step to next line.                   |
| (fresh-line)     | Open line if not on a fresh one.     |
| (close n)        | Close a channel.                     |
| (opendir)        | Open directory and return channel.   |
| (readdir n)      | Read directory.                      |
| (closedir n)     | Close directory.                     |

| Variable | Description            |
|----------|------------------------|
| last-in  | Last input char.       |
| last-out | Last output char.      |
| fnin     | Input channel number.  |
| fnout    | Output channel number. |

### (read): Read expression.
### (print x): Print expression.

### (load pathname): Load and evaluate file.

Reads and evaluates a file expression by expression.  Returns NIL if the
file could not be opened, T otherwise.  LOAD opens an own input channel
alongside the current input channel.

~~~lisp
(load "subseq.lisp")
~~~

LOAD is printing messages.  It can be muted by making \*V?\* non-NIL, or by compiling the
interpreter with option NO\_VERBOSE\_LOAD.

### (require +name): Load missing definition.

### (open pathname mode): Open file and channel.

Opens file at PATHNAME for reading or writing.  MODE must be a symbol.
Returns the channel number or NIL.

| Mode | Description    |
|------|----------------|
|  r   | Read mode      |
|  w   | Write mode     |

Illegal modes cause an ERROR\_FILEMODE.

### (err): Return number of last I/O error or NIL.
### (eof): Tell if last read reached end of file.
### (setin channel): Set input channel.
### (setout channel): Set output channel.
### (in): Read char.

Read character from channel.  It is returned as the character number (e.g.
ASCII).

### (conin): Read console char (non-blocking).

Unlinke IN, CONIN does not wait for input but returns NIL instead when
used with standard input (console, terminal, hoewever you name it).

~~~lisp
; Wait for single character input.
(while (not (eof))
  (!? (conin)
      (return !)))
~~~

### (read-line): Read line as a symbol.

Reads until CR (10) from file, until LF (13) from standard input, not
including them.  Unless reading from standard input, inital LFs are
ignored.

### (putback): Put last read char back to input.

This is the equivalent of C's ungetc(), putting the last character back to
input.  Only one character can be put back.  If one has already been put
back and not read by IN or CONIN, it is overwritten.

### (out x): Print char, string or list of both.

Prints numbers as characters, plain symbol names, also names of other
objects with a name, e.g. built-in functions, and lists of both of them.
Lists may also be nested (contain other lists).

~~~lisp
; Print nothing.
(out)

; Print 'A'.
(out 65)

; Also print 'A'.  (READ converts '\A' to number 65.)
(out \A)

; Print 'Hello world' without double qoutes.
; Finish with a newline instead of using TERPRI.
; But not using TERPRI might not work with your terminal
; (TODO LF/CR explanation for terminals, files and
; operating systems).
(out "Hello world!" 10)

; Print 'TUNIX!'.
(out \T \U "NIX" 33)

; Also print 'TUNIX!'.
(out '(\T \U "NIX" 33))
~~~

### (outlim n): Limit number of chars printed.

### (with-in x +l): Redirect input channel for body.
### (with-out x +l): Redirect output channel for body.

### (terpri): Step to next line.
### (fresh-line): Open line if not on a fresh one.
### (close channel): Close a channel.

### (opendir): Open directory and return channel.

Commodore 8-bit platforms only.

Opens the current directory and returns the channel number, or NIL if no
more channels can be allocated or an error occured.

Then behaviour when reading from a directory channel is undefined.

See: ERR, READDIR, CLOSEDIR

### (readdir n): Read first/next directory from channel.

Commodore 8-bit platforms only.

Returns a list of the format (name size type) or NIL if an error occured.

See: ERR, OPENDIR, CLOSEDIR

### (closedir n): Close a directory channel.

Commodore 8-bit platforms only.  Always returns NIL and never issues an
error.  If a regular channel is applied, the behaviour is undefined.

See: OPENDIR, READDIR, CLOSEDIR

## Time

| Constant   | Description                      |
|------------|----------------------------------|
| +bps+      | Number of bekloppies per second. |

| Variable       | Description                      |
|----------------|----------------------------------|
| \*start-time\* | Number of bekloppies at start.   |

| Function   | Description                      |
|------------|----------------------------------|
| (time)     | Current bekloppie count.         |

Constant +bps+ contains the number of bekloppies per second.

On CBM machines it's currently set to 50, no matter if it's a NTSC or PAL
machine.  Please contribute some auto-detection to file "cbm-common.lisp".

On Unices +BPS+ is 1000 and counting starts with launching TUNIX Lisp.

Function TIME return the current count of bekloppies.  It depends on the
actual machine TUNIX Lisp is running on when the counting from 0 started.

## Raw machine access

| Function   | Description                      |
|------------|----------------------------------|
| (rawptr x) | Get address of object in memory. |
| (peek a)   | Read byte from memory.           |
| (poke a b) | Write to memory.                 |
| (sys a)    | Calls machine code subroutine.   |

### (rawptr a): Read byte from memory.
### (peek a): Read byte from memory.
### (poke a b): Write to memory.
### (sys a): Calls machine code subroutine.

### Example: Print memory dump

~~~lisp
(fn hexdump (from len)
  (dotimes (j (max 1 (/ len 8)))
    (let nrow (min len 8)
      (dotimes (i (min len 8))
        (outhex (peek from) 2)
        (= from (++ from)))
      (= len (- len nrow)))
    (terpri)))
~~~

## Error handling

| Function        | Description                            |
|-----------------|----------------------------------------|
| (quit ?x)       | Return from debugger REPL              |
| (exit)          | Stop program and go to top-level REPL. |
| (error +x)      | Issue a user error like OUT.           |
| (onerror n x x) | User-defined error handler.            |
| (ignore)        | Continue with next REPL expresssion.   |
| (debug)         | Raises a SIGTRAP signal for debugging. |
| (debugger)      | Invoke debugger with next instruction. |

### (quit ?x): Return from debugger REPL.
### (exit): Stop program and go to top-level REPL.

### (error x): Issue a user error.

Prints passed names and numbers like OUT and expressions like PRINT
with prefixed label "USER ERR".

~~~lisp
(with ((haystack nil)
       (needle   t))
  (or (assoc needle *haystack*)
      (error "Cannot find " needle " in haystack.")))
; -> USER ERROR: Cannot find t in haystack.
;    DEBUGGER #1:
;    Error #14: User error
;    ...and so on...
~~~

### (onerror n x x): User-defined error handler.

(See "Advanced error handling".)

### (ignore): Continue with next REPL expression.

*** TODO: Might not be working any more! ***

### (debug): Raises a SIGTRAP signal for debugging.
### (debugger): Invoke debugger with next instruction.

# Quasiquoting

| Form               |     | Description                  |
|--------------------|-----|------------------------------|
| (quote x)          | 'x  | Return X unevaluated.        |
| (quasiquote x)     | $x  | Unevaluated if not unquoted. |
| (unquote x)        | ,x  | Insert into QUASIQUOTE.      |
| (unquote-splice x) | ,@x | Splice into QUASIQUOTE.      |

## (quote x) | 'x: Return X unevaluated.
## (quasiquote x) | $x: Unevaluated if not unquoted.
## (unquote x) | ,x: Insert into QUASIQUOTE.
## (unquote-splice x) | ,@x: Splice into QUASIQUOTE.

# Macro system

| Variable        | Description                 |
|-----------------|-----------------------------|
| \*macros\*      | Simple list of macro names. |

| Function         | Description                     |
|------------------|---------------------------------|
| (macro s a ?+x)) | Add macro function to *macros*. |
| (macro? x)       | Test if symbol is in *macros*.  |
| (macroexpand x)  | Expand expression.              |

A macro is a special form which is replaced by the expression it returns
during 'macro expansion'.  Macros are expanded in the REPL between READ
and EVAL, if the value of MACROEXPAND is a user-defined function.

Defined macros are kept in associative list \*MACROS\*.  Predicate MACRO?
checks if a symbol is a defined macro in that list.

Macros are defined with special form MACRO:

~~~lisp
; Evalue list of expressions BODY and return the result
; of the last.
(macro progn body
  $(block t
     ,@body))
~~~

Macros can also be used inside other macros, so contructs of increasing
complexity can be made from simpler components.

~~~lisp
; Evalute BODY if CONDITION is true.
(macro when (condition . body)
  $(? ,condition
      (progn
        ,@body)))
~~~

## (macro s a +x)): Define macro.

Special form, adding macro S with arguments A and body B to \*MACROS\*.

## (macro? x): Test if symbol is in \*macros\*.

Predicate to test if a symbol denotes a macro in \*MACROS\*.

## (macroexpand x): Expand expression.

# Environment

The environment contains a widely accepted set of functions and macros
known from most other implementations of the Lisp programming languages.

## Local variables

| Macro           | Desscription                        |
|--------------------|-------------------------------------|
| (let n init +b)    | Block with one local variable.
| (with inits +b)    | Block with many local variables.

### (let n init +b): Block with one local variable.

LET is like WITH but creating only one local variable for its body.

~~~lisp
(var x 'a)
(let x 'b
  (print x))    ; Prints 'b'.
(print x)       ; Prints 'a'.
~~~

### (with inits +b): Block with many local variables.

## Control flow macros

| Macro                | Description                      |
|----------------------|----------------------------------|
| (!? cond conseq...)  | ?, assigning cond to !.          |
| (prog1 +b)           | Return result of first.          |
| (progn +b)           | Return result of last.           |
| (when cond +b)       | Evaluate if condition is true.   |
| (awhen cond +b)      | WHEN, assigning cond to !.       |
| (case x v conseq...) | ?, using values insted of cond.  |
| (unless cond +b)     | Evaluate if condition is false.  |

### (prog1 +b): Return result of first.

### (progn +b): Return result of last.

### (when cond +b): Evaluate body if condition is true.

~~~lisp
(when (hungry? coder)
  (feed coder)
  (tell coder "That's 5.99!"))
~~~

replaces

~~~lisp
(? (hungry? coder)
   (block t
     (feed coder)
     (tell coder "That's 5.99!")))
~~~

### (case x +l): Evaluate conditionally by matching value.

Evaluates conditionally by matching values in pairs.  EQL is used as the
matching predicate.

~~~lisp
(case x
  'a  (out "X is A.")
  5   (out "X is 5."))
~~~

It can have an optional default.

~~~lisp
(case x
  'a  (out "X is A.")
  5   (out "X is 5.")
  (out "X is neither A or 5."))
~~~

This is the macro-expanded version (TMP would be an anonymous symbol):

~~~lisp
(let tmp x
  (eql tmp 'a) (out "X is A.")
  (eql tmp 5)  (out "X is 5.")
  (out "X is neither A or 5."))
~~~

### (unless cond +b): Evaluate if condition is false.

## Lists

| Function          | Description                         |
|-------------------|-------------------------------------|
| (dup x n)         | Duplicate X N times.                |
| (list +x)         | Return list evaluated.              |
| (list? x)         | Test if argument is NIL or a cons.  |
| (cadr l)...       | Nested CAR/CDR combinations.        |
| (carlist l)       | Get first elements of lists.        |
| (cdrlist l)       | Get rest elements of lists.         |
| (append +l)       | Copy and append lists.              |
| (copy-list x)     | Copy list.                          |
| (copy-tree x)     | Copy recursively.                   |
| (count-if f l)    | Count by predicate.                 |
| (cut-at n l)      | Destructively split list.           |
| (ensure-list x)   | Turn atom into list.                |
| (every f x)       | Test if F is T for all X.           |
| (find x l)        | Find element X in list.             |
| (find-if f l)     | Find element X in list by F.        |
| (group l n)       | Split L in lists of length N.       |
| (nth n l)         | Get Nth element of list.            |
| (nthcdr n l)      | Get Nth cons of list.               |
| (mapcar f +l)     | Map CARs of lists.                  |
| (mapcan f +l)     | Concatenating MAPCAR.               |
| (member-if f l)   | Find cons with element in list.     |
| (remove-if f l)   | Removed elemnts from list.          |
| (reverse l)       | Reverse list.                       |
| (position x l ?f) | Find position of object in list.    |
| (split x l ?f)    | Split list where object occurs.     |
| (subseq l n n)    | Return sublist.                     |

### (dup x n): Duplicate X N times

#### Example

~~~lisp
(dup 'x 3) ; -> (x x x)
~~~

### (list +x): Make list from arguments

### (list? x): Test if argument is NIL or a cons

### (cadr l)...: Nested CAR/CDR combinations

As inconvenient as the names CAR and CDR are, as refreshingly practical
combinations of them appear to be:

~~~lisp
(fn cadr (x)
  (car (cdr x)))
(fn caddr (x)
  (car (cdr (cdr x))))
~~~

### (carlist l): Get first elements of lists

### (cdrlist l): Get rest elements of lists

### (append +l): Copy and append lists

### (copy-list x): Copy list

### (copy-tree x): Copy recursively

### (count-if f l): Count by predicate

~~~lisp
(count-if number? '(l 1 5 p)) ; -> 2
~~~

### (cut-at n l): Destructively split list at position.

Sets the CDR of the cons before that position of the list passed to it to
NIL.

~~~lisp
(let l '(l i s p)
  l              ; -> (l i s p)
  (cut-at 2 l)   ; -> (s p)
  l)             ; -> (l i)
~~~

It's vital to note that a position of 0 will cause the list to be returned
as is, and that original list will be still intact and not NIL.  That
makes CUT-AT less of a lispy function.

~~~lisp
(let l '(l i s p)
  l              ; -> (l i s p)
  (cut-at 0 l)   ; -> (l i s p)
  l)             ; -> (l i s p)
~~~

### (find x l): Find element X in list.

### (nth n l): Get Nth element of list.

~~~lisp
(nth 0 '(l i s p)) ; -> l
(nth 2 '(l i s p)) ; -> s
~~~

### (nthcdr n l): Get Nth cons of list.

NTHCDR returns the nth cons of a list, not the CAR of it, like NTH does.

~~~lisp
(nthcdr 0 '(l i s p)) ; -> (l i s p)
(nthcdr 2 '(l i s p)) ; -> (s p)
~~~

### (position x l ?f) | Find position of X in L.

Returns the position of X in list L, starting with 0 for the first
position, or NIL if X has not been found.  Predicate F is EQL by default.

~~~lisp
(position 'foreign '(mom dad)) ; -> nil
(position 'mom '(mom dad))     ; -> 0
(position 'dad '(mom dad))     ; -> 1
~~~

### (split x l ?f) | Split list at object.

Splits list L where object X occurs, tested by predicate F, which is EQL
by default.  The object is removed from the list.

~~~lisp
(split 'b '(a a b c c c)) ; -> ((a a) (c c c))
(split 'b '(a a b b c c)) ; -> ((a a) nil (c c))
~~~

### (subseq l start ?end): Get sublist.

Copies part of a list into a new one from 'start' to 'end'.  If 'end' is
not given, everything to the end of the list is taken.  'start' and 'end'
begin with number 0 for the first position in the list.  'end' is
exclusive, telling with which position copying finishes.

~~~lisp
(subseq '(t u n i x) 2)   ; -> (n i x)
(subseq '(t u n i x) 0 2) ; -> (t u)
;         0 1 2 3 4
~~~

## Looping

| Macro                                | Description                   |
|--------------------------------------|-------------------------------|
| (do (+iters (brk-cond res)) . body)  | Generic loop.                 |
| (do* (+iters (brk-cond res)) . body) | Generic loop.                 |
| (dolist (iter init) . body)          | Loop over list elements.      |
| (dotimes (iter n) . body)            | Loop N times.                 |
| (while (cond x) +b)                  | Loop while condiiton is true. |
| (awhile (cond x) +b)                 | WHILE with condition in "!".  |

###  DO/DO\*: Generic loop.

Macro DO is used for iterative looping.  It allows for the
execution of a block of code repeatedly, with controlled variable
initialization, step updates, and termination conditions.

DO* addtionally allows step updates to reference previously defined
loop variables.

#### Form

~~~lisp
(do ((var1 init1 step1)
     (var2 init2 step2) ...)
    (exit-condition result)
  body)
~~~

- VAR, INIT, STEP: VAR is initialized to INIT, and after each iteration,
  it is updated by STEP.
- EXIT-CONDITION: The loop terminates when the `exit-condition`
  evaluates to true, returning `result`.
- BODY: The block of code that executes on each iteration until the
  `exit-condition` is satisfied.

#### Description

DO is one of the most versatile constructs in Lisp, combining the
functionality of of multiple loop constructs into a single, elegant
tool.  Most other constructs like DOLIST, DOTIME, WHILE, and so on,
are implemented using DO.

#### Examples

##### Reversed DOMTIMES

~~~lisp
(do ((i 0 (+ i 1))) 
    ((== i 10) i)
  (print i))
; -> 0 1 2 3 4 5 6 7 8 9
~~~

I is initialized to 0, incremented in each iteration, and the loop
exits when I equals 10, returning I.

##### Iterating over a list's conses

Iterating over conses is required if list elements ahead need to be
token into consideration.

~~~lisp
(fn butlast (x)
  (do ((c x (cdr c)))
      ((not c))
    (or (cdr c)
        (return c))))
~~~

##### Iterating over a list's conses with indexing

~~~lisp
(do ((c x (cdr c))
     (i 0 (++ i)))
    ((not c))
  (let e (car c)
    BODY))
~~~

### (dolist (iter init) . body): Loop over list elements.

### (dotimes (iter n) . body): Loop N times.

#### Syntax

~~~lisp
(dotimes (var number ?result)
  body)
~~~

Counts down from N to 0, evaluation BODY each time.   A non-negative N is an
error.

#### Example: Make list of duplicates

~~~lisp
(fn dup (x n)
  (aprog1 nil
    (dotimes (i n)
      (push x !))))

(dup 'x 3) ; -> (x x x)
~~~

### (while (cond x) +b): Loop while condiiton is true
### (awhile (cond x) +b): WHILE with condition in "!"

## Stacks

| Macro      | Description                      |
|------------|----------------------------------|
| (push x l) | Destructively push onto stack L. |
| (pop l)    | Destructively pop from stack L.  |

Stacks are lists to which elements are pushed to or popped off the front.

### (push x l): Destructively push onto stack L.

~~~lisp
(var x '(2))
(push 1 x) ; '(1 2)
x ; '(1 2)
~~~

### (pop l): Destructively pop from stack L.

~~~lisp
(var x '(1 2))
(pop 1 x) ; '(1)
x ; '(2)

~~~

## Queues

| Function           | Description                   |
|--------------------|-------------------------------|
| (make-queue)       | Make queue.                   |
| (enqueue c x)      | Add object X to queue C.      |
| (queue-list c)     | Return list of queue.         |
| (queue-pop c)      | Pop element from queue front. |
| (with-queue s . b) | Make local queue.             |

### (make-queue): Make queue.

### (enqueue c x): Add object X to queue C.

## Sets

| Function               | Description                    |
|------------------------|--------------------------------|
| (unique x)             | Make list a set.               |
| (adjoin x set)         | Add element to set.            |
| (intersect a b)        | Elements in both.              |
| (set-difference a b)   | B elements that are not in A.  |
| (union a b)            | Unique elements from both.     |
| (set-exclusive-or a b) | Elements that are not in both. |
| (subseq? a b)          | Test if A is subset of B.      |

### (unique x): Make list a set.

### (adjoin x set): Add element to set.

### (intersect a b): Elements in both.

### (set-difference a b): B elements that are not in A.

### (union a b): Unique elements from both.

### (set-exclusive-or a b): Elements that are not in both.

### (subseq? a b): Test if A is subset of B,

## Associative lists

| Function         | Description                        |
|------------------|------------------------------------|
| (acons alist c)  | Add key/value to associative list. |
| (assoc x l)      | Return list that start with X.     |
| (aremove x l)    | Remove X from associative L.       |
| (aremove-if f l) | Remove by F from associative L.    |

A list of lists where the first element is the key and the
rest is the value.

### (acons alist c): Add key/value to associative list.
### (assoc x l): Return list that start with X.
### (aremove x l): Remove X from associative L.
### (aremove-if f l): Remove by F from associative L.

## Console control

### (clrscr): Clear screen.
### (con-xy x y): Set cursor position.
### (con-crs x): Set cursor visibility.
### (con-rvs x): Set reverse mode.
### (con-direct x): Set direct mode.
### (con-x): Get cursor X position.
### (con-y): Get cursor Y position.

## Version information

| Variable | Description                             |
|----------|-----------------------------------------|
| +target+ | Target machine the interpreter runs on. |
|   +v+    | Git tag and short SHA.                  |
|   +vb+   | Git branch.                             |
|  \*v?\*  | Tell if definitions should be printed.  |

## Autoloader

| Function   | Description             |
|------------|-------------------------|
| (autoload) | Load missing procedure. |

| Variable | Description                              |
|----------|------------------------------------------|
| \*alx\*  | Name translation map (associative list). |
| \*alv?\* | Verbose mode during AUTOLOAD.            |

Missing procedures are loaded on demand by AUTOLOAD (assigned to ONERROR), as long as the filename is the same
(plus the ".lsp" suffix).
Messages are not printed during AUTOLOAD unless \*ALV?\* is not NIL.
The default is T (verbose).

~~~lisp
; Verbose autoloading of LS.
(with-global *alv?* t
  (ls))
Loading ls.lsp
~~~

If the missing procedure is a macro, the original code is expanded on the
spot.

For names with wildcards name translations can been added to \*ALX\*.
Can be used to map many procedures to a single file.

~~~lisp
; Add translation for macro "!?", which is in file "aif.lsp".
; (Already done.)
(push (cons '!? 'aif) *alx*)
~~~

## Maintainance

| Function          | Description                       |
|-------------------|-----------------------------------|
| (source s)        | Get definition of symbol.         |
| (compress-tree x) | Find and replace double subtrees. |

### (source s): Print definition of a symbol.

Prints a definition that can be used to re-define the symbol later, e.g.
by writing it to a file for LOAD.

### (compress-tree x): Find and replace double subtrees.

***COMPRESS-TREE Cannot be used with compressed conses.***

This is about finding duplicate subtrees in the heap and replacing them by
referencing the "original".  This might pay out very well, although it is
computationally demanding and is best performed in the background if the
machine is otherwise idle.

This function should only be used on modern machines or in acclerating
emulators (e.g. "warp mode" in VICE).

Here is how to compress all of the heap:

~~~lisp
(compress-tree *universe*)
~~~

#### The algorithm

This algorithm assumes, that no-one else will modify the trees in the
heap.  If lists are created, the algorithm cannot work alongside.

Two iterators are being used.  The first starts at the root of the tree
and traverses in CAR direction.  With each cons a second iterator goes
through the rest of the tree, starting with the CDR of the cons and
comparing each cons with the one where the first iterator resides.  It is
also traversing back to the top of the tree to do the same with the CDRs
of the conses which have already been visited.  Then the first iterator
steps on and the process repeats until both iterator came back to the root
of the tree.

The movement of the first iterator could be implemented using recursion.
The application of the second iterator travelling up the same path
prohibits that.  Instead, a path list has to be created by the first
iterator which the second one can use to travel back down to the root.
From each cons on it can use regular recursion to travel through the rest
of the tree.

Essentially the first iterator is implemented as function COMPRESS-TREE
and the second iterator as REPLACE-DUPLICATES.

## Target information

Constant +TARGET+ identifies the target machine, which is one of:

* c128
* c16
* c64
* pet
* plus4
* unix
* vic20

## UNIX networking sockets

| Built-in                       | Description                 |
|--------------------------------|-----------------------------|
| (socket-connect s n)           | Open IP/port.               |
| (socket-send n s)              | Write symbol to socket.     |
| (socket-read n)                | Read symbol from socket.    |
| (socket-block n s)             | Select blocking I/O.        |
| (socket-listen n)              | Listen on port.             |
| (socket-accept n)              | Accept incoming connection. |
| (socket-close s)               | Close socket.               |

Available on systems that support compile-time option HAVE\_SOCKETS.
Only very basic support, no polling or listening.

~~~lisp
(!? (socket-connect "127.0.0.1" 8000)
    (progn
      (socket-send ! (symbol (append (symbol-name "GET / HTTP/1.1")
                                     (list 13 10 13 10))))
      (print (socket-read !))
      (socket-close !))
    (message "Cannot open 127.0.0.1:8000."))
~~~

### (socket-accept n): Accept incoming connection.

Returns T if the call was successful but no connection is waiting.

# Compile-time options to add or remove features

## COMPRESSED\_CONS: Per-GC list compression.

When enabled by compile-time option COMPRESSED\_CONS, storing the CDR of a
cons can be spared if that is following immediately on the heap.  Since
that makes compressed conses immutable (you cannot use SETCDR on them),
compression is performed if the garbage collector was called by the
program and not the allocator.  The GC is also called to compress conses
if the available heap left is smaller than the number of bytes specified
by compile-time option GC\_AFTER\_LOAD\_THRESHOLD, which is 2048 by
default.

If you want to make sure that all conses that can be are compressed you
have to call the garbage collector twice.

Compile-time option VERBOSE\_COMPRESSED\_CONS is set, the GC will print a
'C' to the currently active output channel.

## EXIT\_FAILURE\_ON\_ERROR

# Internals

## Garbage collection

The compacting mark-and-sweep garbage collector first traces and marks all
objects that are in use, starting with \*UNIVERSE\*, and moves the used
objects together in the sweep phase, overwriting the unused ones.  Finally
it relocates all pointers to moved objects.  The more gaps have to be
removed, the longer the relocation phase takes, because adjacent gaps are
merged.  On small machines removing all macros and doing a manual GC is
recommended before program start because macro-expansions create lots of
gaps.

## Heap object layouts

You can get the memory address of any object with RAWPTR.  You can then
use it to PEEK and POKE memory directly.

On 32-bit and 64-bit architecture pointers and numbers are four or eight
bytes in size.  The following tables show the layouts for 16-bit systems.

All objects start with a type byte:

| Bit | Description                                   |
|-----|-----------------------------------------------|
|  0  | Type bit for conses                           |
|  1  | Type bit for numbers                          |
|  2  | Type bit for symbols                          |
|  3  | Type bit for built-in functions               |
|  4  | Extended type (special form, compressed cons) |
|  5  | Unused                                        |
|  6  | Unused                                        |
|  7  | Mark bit for garbage collection               |

### Cones

| Offset | Description         |
|--------|---------------------|
|   0    | Type info (value 1) |
|   1-2  | CAR value           |
|   3-4  | CDR value           |

### Numbers

| Offset | Description         |
|--------|---------------------|
|   0    | Type info (value 2) |
|   1-4  | Long integer[^long] |

[^long]: Will occupy eight bytes on 64-bit systems.

### Symbols

| Offset | Description                         |
|--------|-------------------------------------|
|   0    | Type info (value 4 or 20)           |
|   1    | Name length (0-255)                 |
|   2-3  | Pointer to next symbol for look-ups |
|   4-x  | Name (optional)                     |

Adding the extended type bit turn a symbol to a special form.  Arguments
its function won't be evaluated then.

### Built-in functions

Built-ins are symbols with a pointer to a descriptor of the built-in.  It
contains an ASCIIZ pointer to the name, another to the character-based
argument definition and the address of its implementation.

| Offset | Description                         |
|--------|-------------------------------------|
|  0     | Type info (value 8)                 |
|  1-2   | Symbol value                        |
|  3-4   | Pointer to next symbol for look-ups |
|  5     | Name length (0-255)                 |
| (6-x)  | Name (optional)                     |

# Adding a new target

## 1. Add a supplementary compiler configuration (cc65)

Copy the configuration file of the desired platform from
'src/contrib/cc65/cfg' to 'cfg/ld65/', and to 'src/bin/lisp/cc65-cfg/'.
You'll have to tweak the latter to provide enough zeropage locations, and
add these segments:

~~~
CODE_BUILTIN:  load = MAIN, type = ro;
CODE_BUILTINS: load = MAIN, type = ro;
CODE_ERROR:    load = MAIN, type = ro;
CODE_EVAL:     load = MAIN, type = ro;
CODE_GC:       load = MAIN, type = ro;
CODE_HEAP:     load = MAIN, type = ro;
CODE_IO:       load = MAIN, type = ro;
CODE_IMAGE:    load = MAIN, type = ro;
CODE_LIST:     load = MAIN, type = ro;
CODE_PRINT:    load = MAIN, type = ro;
CODE_READ:     load = MAIN, type = ro;
CODE_REPL:     load = MAIN, type = ro;
CODE_SLOW:     load = MAIN, type = ro;
CODE_INIT:     load = MAIN, type = ro;
~~~

## 2. Add target to build system (GNU make)

If it's a new compiler, add a new variable with the new target's name to
'src/mk/Makefile.targets'.  For example, if that compiler is called
"MagiC", and the new target is a "SlideRule", add

~~~
MAGIC_TARGETS = sliderule
~~~

## 3. Create build files for the compiler compiler.

Copy one of files 'src/mk/Makefile.config.\*' to
'src/mk/Makefile.config.magic' and adapt it to that brand new, incredible
MagiC compiler.  It should contain alls flags wants to play around with.
These are used by the new file 'src/mk/Makefile.build.magic', which you
can create the same way.

## 4. Add C preprocessor definition for the target

Add the preprocessor definition of TARGET\_SLIDERULE to the end of
'src/mk/Makefile.config'.  You will need it to make changes in the C code
of TUNIX Lisp.

## 5. Add default compile-time configuration.

These are in the head of 'src/lib/lisp/liblisp.h'.  Again, copy one that
suits you best.  For our SlideRule it might look like this:

~~~C
// Calculation Circus Co. "SlideRule 2000"
#ifdef TARGET_SLIDERULE

// Save as much memory as can be.
#define SLOW
#define NO_DEBUGGER

#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       256
#define RELOC_TABLE_ENTRIES 128
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif
~~~

## 6. Add libsimple I/O.

Add 'libsimpleio-stdlib' for your target to 'src/bin/lisp/Makefile'.  You
may come up with your own, platform-specific libsimpleio add-on for the
SlideRule.

## 7. Build, fix, build, fix...

Try to build for your target...

~~~sh
make worldclean world TARGET=sliderule
~~~

...and share your thoughts with us!
