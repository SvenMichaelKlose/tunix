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

TUNIX Lisp is a highly efficient and scalable Lisp
interpreter, written in ANSI-C.  It is designed for
constrained environments, such as classic home computers
(including 6502-based systems), microcontrollers and
embedded systems, but also has its place on modern
machines.

Features:

* Interactive REPL.
* Debugger with stepping and breakpoints.
* User-defined error handlers.
* Lean I/O interface.[^io]
* Compacting mark-and-sweep garbage collector.[^gc]
* Supplementary compressed stack.[^stack]
* List compression.
* Saving and loading system images.
* Unified symbol and string handling with data compression
  features.
* UNDER CONSTRUCTION: Integrated text editor.

Most features can be left out during compilation to make
TUNIX Lisp fit even the most contrained environments.

[^gc]: Planned to be made interruptible to some degree,
  optionally truly interruptible providing a copying garbage
  collector.
[^io]: Instead of providing a file number for each I/O
  operation an input and/or output channel must be selected
  beforehand, bridging the gap between plain standard I/O
  and multi-stream handling without making the API more
  complex from the start, supporting operation in maximally
  constrained environments.
[^stack]: It holds byte-sized tags instead of larger return
  addresses.  Also to support architectures with limited CPU
  stacks, like those with MOS-6502 CPUs.

This distribution builds executables for these platforms
using the cc65 C compiler suite:

* Commodore C128
* Commodore C16
* Commodore C64
* Commodore Plus4
* Commodore VIC-20 (+27K)

It also compiles on regular Unixoids, using the GNU compiler
toolchain or compatibles.

## Differences to other dialects

The TUNIX Lisp dialect is very much like any other.  Here
are some things that raise an eyebrow when seeing them the
first time:

| Most other dialects    | TUNIX Lisp      |
|------------------------|-----------------|
| backquote sign '`'     | dollar sign '$' |
| (RPLACA c v)           | (SETCAR c v)    |
| (RPLACD c v)           | (SETCDR c v)    |
| (MAKE-SYMBOL x)        | (SYMBOL l)      |
| (SYMBOL-VALUE s)       | (VALUE s)       |
| (FILTER f l)           | (@ f l)         |
| (LAMBDA (args . body)) | (args . body)   |
| #\\A                   | \\A             |

Because the backquote (`) is not part of the charsets of old
machines TUNIX Lisp intends to support, the dollar sign ($)
is used as the abbreviation for QUASIQUOTE.

MEMBER and FIND are comparing with EQ instead of EQL as
these functions are used internally as well and need to be
fast.  Use NEMBER-IF or FIND-IF together with EQL to match
numbers by value.

LAMBDA is not around yet.  Function expressions are quoted
when used as arguments to other functions.  That makes
compiling them to 'native' function impossible, so something
similar will have to be in later versions.

### Symbols are strings

Symbols have a case-sensitive name and a value and they also
serve as strings.  They can be converted to and from
character value lists:

~~~lisp
(symbol '(\A \B \C)) -> "ABC"
~~~

Symbols may also be anonymous, with no name at all.

~~~lisp
(symbol) ; Anonymous symbol that won't get re-used.
~~~

SYMBOL will issue an error if it is passed a dotted pair.

## Memory consumption

### Heap

Object allocation is fast, requiring bumping up the pointer
to the top of the growing heap, and a boundary check to
trigger garbage collection when the heap is full.

| Data type              | heap  |
|------------------------|-------|
| cons                   |   5   |
| number (32 bit signed) |   5   |
| symbol (also string)   | 4-260 |

### CPU stack, object stack, and tag stack

Alongside the CPU stack a separate garbage-collected object
stack holds function arguments and objects that need to be
relocated during garbage collection.  An additional raw
stack holds return tags of byte size instead of full return
addresses, and raw pointers to built-in procedure's argument
definitions.

### Inevitable creation of list elements

APPLY copies all arguments but the last one.

# Installing binaries

"Installing" a binary release is the easiest way to go
exploring.  I'd rather recommend compiling it yourself.

## Getting a release

Download the latest binary from
[https://github.com/SvenMichaelKlose/tunix/releases](https://github.com/SvenMichaelKlose/tunix/releases).

The name of the ZIP file contains the project's name
"tunix", followed by its release version, ID in the
public Git repository (short SHA hash), and finally the
release data, followed by the opligatory ZIP suffix.
It should look like this:

~~~
tunix.v0.0.5+bca5411.2024-08-22.zip
       ^^^^^ ^^^^^^^ ^^^^^^^^^^
         |      |        |
         |      |   release date
         |   Git SHA
      version
~~~

The version, "0.0.5" in this case, contains a major, minor
and patch version according to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).
TUNIX Lisp is a bit different as it has a major version
number of "0", indicating that it's not meant for production
where you expect things to not change from one day to the
other.  To make it even worse, the minor version also being
"0" means that absolutely everything could change, no matter
if there'll be hell or high water.  It's about making TUNIX
grown up enough to be able to follow the Semantic Versioning
rules in the first place.  But we're trying to keep the pain
away.  The patch level increases with every release - that
happens if a bunch of changes happened that makes everyones
life easier.  You must download the latest.  The others are
kept for the protocol only.

Things you must expect to change sooner than later:

- Required special keyword (like the notoriuos LAMBDA) to
  tell function expressiod from regular expressions.  It'll
  be required to make the language more comfortable when it
  comes to lexical scope, and to have a compiler produce
  effective code.

## Unpacking

Download the latest release and unpack it.  On a unixoid
command-line (Linux/Mac) this should do:

~~~sh
unzip tunix.v
~~~

It contains directory "tunix" and subdirectories for
all supported platforms, e.g. "tunix/c64".  You can step
into one of those and run TUNIX Lisp in your favourite
emulator, or you can also transfer the files to your
platform, depending on what it supports.  For Commdore
8-bit machines there are SD card readers, also known as
SD2IEC drives, available.

### Running on Linux/Mac/BSD, etc.

If you're running a something Unixoid, step into directory
"tunix/unix" and shoot it up by typing "./lisp":

~~~sh
cd tunix/unix
./lisp
~~~

You have to step into "tunix/unix" or TUNIX Lisp won't find
the other files it needs to get going.

### Installing VICE and YAPE mmulators

The "VersatIle Commodore Emulator" is the most popular one
for Commodore 8-bit machines.  Commodore C16 and Plus/4
fanatics will insist on using YAPE as it's more compatible
to the original.

#### The VersatIle Commdore Emulator (VICE)

##### **Linux**

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

If VICE is not available in your distribution's repositories, you may need to compile it from source. Visit the [VICE website](https://vice-emu.sourceforge.io/) for more information.

##### **macOS**

VICE can be installed via Homebrew on macOS:

- **Using Homebrew**:
  ```sh
  brew install vice
  ```

Alternatively, you can download the latest macOS binary from
the [VICE website](https://vice-emu.sourceforge.io/)
and follow the instructions provided there.

##### **Windows**

For Windows, you can download the latest VICE binary from
the [VICE website](https://vice-emu.sourceforge.io/).
After downloading, extract the archive to a directory of
your choice and run the appropriate executable (e.g.,
`x64.exe` for C64 emulation).

#### Yet Another Plus/4 Emulator (YAPE)

##### **Windows**

YAPE is primarily a Windows-based emulator.  You can
download it from the
[YAPE website](http://yape.homeserver.hu/).
After downloading, extract the archive and run `yape.exe`.

##### **Linux and macOS**

YAPE is not natively available for Linux or macOS, but you
can run it using Wine, a compatibility layer for running
Windows applications on Unix-like operating systems.

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

#### Additional Resources

For more detailed installation instructions or
troubleshooting, please refer to the respective emulator's
website:

- [VICE Emulator](https://vice-emu.sourceforge.io/)
- [YAPE Emulator](http://yape.homeserver.hu/)

## Building TUNIX from source

Building TUNIX from source code is highly recommended if you
want to stay up to date, especially for getting patches that
remove bugs - naturally these occur often in early software.

### General instructions for all platforms

#### Cloning the Repository

To begin, clone the TUNIX repository:

```bash
git clone https://github.com/svenklose/tunix.git
cd tunix
```

#### Fetching third-party code

After cloning the repository, you must fetch all the
required third-party code and build that first:

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

# User interface: The READ/EVAL/PRINT-Loop (REPL)

The REPL is the user interface.  It prompts you for input by
printing an asterisk '\*' in its regular mode, except on
Commodore 8-bit machines, to allow using the KERNAL's screen
editor.  After reading an expression it is evaluated and the
result of that evaluation is output.  Then it starts over,
prompting you for the next expression.

REPLs can be nested, e.g. when an error occured, a new REPL
is launched in debug mode.  With that you can examine the
environment, execute code step by step and present a
correct alternative for a faulty expression.

Any REPL, no matter its mode, can be terminated using the
built-in QUIT function, which takes a return value for the
REPL as its argument.  It may become the return value of
a LOAD function when loading a file, or the alternative
return value of a faulty expression in debug mode.
Built-in function IGNORE interrupts evaluation of the
current expression read by the active REPL to start over
with reading and evaluating the next one.

Built-in function EXIT stops the program and returns to the
topmost REPL.  When passed an exit code, EXIT terminates the
interpreter and returns to the operating system.

# Definiton of permanent symbols

FN and VAR assign expressions to symbols which are also
added to variable \*universe\*, a list of symbols the
garbage collector starting off with to reach all used
objects.  The difference between FN and VAR is that VAR
evaluates its initialization argument and FN assigns its
argument list unevaluated.

~~~lisp
; Define permanent, named function.
(fn welcome ()
  (out '"Hello World!")
  (terpri))

; Define permanent, named variable.
(var x nil)
~~~

If you are using a screen editor, the SOURCE function is
rather useful.  It returns a defining expression for any
symbol.

# Functions

Functions are lists starting with an argument definition
followed by a list of expressions.  The result of the last
expression is returned.

The LAMBDA keyword is not around at the moment but it has
to be to make the compiler work.

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

Anonymous functions can be used as the first element of an
expression without quoting:

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

The QUASIQUOTE (short form "$") can be used to emulate
read-only lexical scope by unquoting outer values:

~~~lisp
; Make a function that adds X to its argument.
(fn make-adder (x)
  $((a)
     (+ a ,x)))
~~~

## Argument type descriptions (and definitions)

Built-in functions have character-based and typed argument
definitions.  They are also used, padded with spaces, to
describe arguments in this manual for all procedures
(functions, macros and special forms).

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

TUNIX Lisp boils I/O down to its basics: one channel for
input and one for output, initially wired to "standard I/O",
like your terminal with screen and keyboard.  Input and
output can each be switched to other channels.  If you
launch a LOAD command to execute a Lisp file, the input
channel is connected to that file until it's been read
entirely, but in general a channel can be directed to
another one anytime.

## READing and PRINTing expressions

Expressions can be read and written using built-in functions
READ and PRINT.  Strings and chars have dedicated formats:

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

An input and an output channel can be switched between open
streams separately using functions SETIN and SETOUT.
Symbols STDIN and STDOUT contain the standard I/O channel
numbers.

~~~lisp
; Switch to standard I/O channels.
(setin stdin)
(setout stdout)
~~~

The currently active channels numbers are in symbols FNIN
and FNOUT.

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

# Debugging and advanced error handling

The debugger is invoked in case of an error unless ONERROR
has been defined.  Beatiful things can be done by handling
errors automatically, but let's get our hands on the
debugger first.

## The debugger REPL

| Variable | Description                            |
|----------|----------------------------------------|
|   *b*    | List of symbols that are breakpointed. |
|   *r*    | Initial return value of current REPL.  |

The debugger is the REPL in debug mode.  It prints a status
info before waiting for user input, so you know where the
program execution has been interrupted.  It has this format:

~~~
Debugger <number of nested debuggers>:
Error #5: <reason for break>
Rvalue: <last expression's (and debugger's) return value>
In:
<top-level expression with current one highlighted>
~~~

The debugger takes expressions like the regular REPL, plus
some commands consisting of a single character to step
through the code conveniently.  If another error occurs,
yet another debugger REPL will be invoked and the "number of
nested debuggers" incremented.

The current expression is either the one that failed, or the
one that will be evaluated next in cause the debugger
stopped at a breakpoint (and no error number and description
is shown).

The return value of the debugger will change with every
expression you enter, except when using aforementioned short
commands.  In case of an error, that's the value you want to
replace with a valid one before continuing program
execution.  Symbol \*R\* contains the return value when the
debugger was invoked, should you want to see or use it again
although you've replaced it already – just enter "*r*" and
it'll be restored.

These are the available short commands:

| Command | Description                                    |
|---------|------------------------------------------------|
| c       | Continue program execution.                    |
| q       | Stop execution, returning to top level REPL.   |
| s       | Step into user-defined procedure.              |
| n       | Execute current expression in whole.           |
| pX      | Evaluate and print expression X.  (No macros!) |
| bS      | Set breakpoint on procedure S.                 |
| b       | Print breakpoints.                             |
| dS      | Delete breakpoint on procedure S.              |
| d       | Delete all breakpoints.                        |

Command "p" evaluates the expression immediately following
it.  A macro expansion is *not* performed and it'll *not*
change the debugger's return value.

## Stepping through the code

Short command 's' will step to the next argument of the
current expression, evaluation what's on the way or enter
the currently highlighted function if all arguments have
been dealt with.  With 'n' the function and all its
arguments are evaluated, taking you to the next expression
in the list.  If you had it the program, you can exit it
with short command 'q' and take a break yourself.

IDEA:
* step into newly entered expression
* step into restarted expression.  Already changed values
  are a problem then.

## Breakpoints

Global variable \*B\* is a list procedures' names which, if
called, will invoke the debugger.

You can modify \*B\* using the regular set of procedures:

~~~lisp
; Set breakpoint on procedure SUBSEQ.
(push 'subseq *b*)

; Delete a breakpoint.
(= *b* (remove 'subseq *b*))

; Delete all breakpoints.
(= *b* nil)
~~~

Inside the debugger REPL that's inconvenient as every
regular expression changes the debugger's return value.  Use
short commands 'b' and 'd' instead.

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

If defined, user-defined function ONERROR is called on
errors, except for internal ones which halt the interpreter
to avoid unexpected behaviour.  Errors happening inside
ONERROR will cause it to be called again.  The handler
must return a correct replacement value insted of calling
QUIT or use IGNORE.

ONERROR is called with the error code, the current REPL
(top-level) expression, and the faulty expression within
it:

IDEA: Calling DEBUGGER inside ONERROR if the error cannot
be dealt with.

~~~lisp
(fn onerror (errcode repl faulty)
  (out "ONERROR handler called!")(terpri)
  ; We don't handle errors so unleash the debugger on it.
  'debugger)
~~~

### Error codes

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
| FILEMODE        | 13   | Illegal mode for OPEN.          |
| USER            | 14   | ERROR function was called.      |
| INTERNAL        | 15   | Returned to operating system.   |

#### ERROR\_OUT\_OF\_HEAP

Returns to the current REPL and does a garbage collection
before calling an ONERROR handler or debugger.

Compile-time option ONETIME\_HEAP\_MARGIN specified the
number of heap bytes that are kept for calling an ONERROR
handler.

# Built-in functions

## General

| Function   | Description                            |
|------------|----------------------------------------|
| (gc)       | Free unused objects.                   |
| (exit ?n)  | Exit program or interpreter with code. |

### (gc): Free unused objects.

Triggers the garbage collector.  It marks all objects
linked to variable \*UNIVERSE\*, compacts the heap and
relocates all pointers.

### (exit ?n): Exit program or interpreter with exit code.

When called without arguments the program is stopped and
control is returned to the top-level REPL.  When called with
a number that number is the exit code for the interpreter
which will terminate immediately.

## Definitions

| Form                         | Type                      |
|------------------------------|---------------------------|
| (var 'name x)                | Define symbol with value. |
| (fn 'name 'args '+body)      | Define function.          |
| (special 'name 'args '+body) | Define special form.      |

| Function   | Description                              |
|------------|------------------------------------------|
| (source s) | Return defining expression for a symbol. |

### (fn 'name 'args '+body): Define permanent, named function.

### (special 'name 'args '+body): Make special form.

Special forms are functions that take their arguments
unevaluated, e.g. QUASIQUOTE and MACRO, so you don't have to
quote arguments of that function manually.

### (var 'name init): Define permanent, named variable.

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

Returns argument unevaluated.  Suppresses replacing symbols
by their values on evaluation.

~~~lisp
; Define variable X, containing the string "What a day!".
(var x "What a day!")
x         -> "What a day!"
(quote x) -> x
'x        -> x  ; Short form.
~~~

### (apply fun . args): Apply function.

Calls function FUN.  Unlike the rather straightforward
FUNCALL, which takes its arguments as provided, APPLY
expects the last element of ARGS to be a list, which is
then appended to the previous elements:

~~~lisp
(fn list x
  x)

(apply list '(10 11))   -> (10 11)
(apply list 1 2 '(3 4)) -> (1 2 3 4)
~~~

The reason for this is that it takes away the need to do
that kind of concatenation oneself repeatedly, which would
happen a lot otherwise.

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

Returns the second argument if the first one evaluates to
non-NIL.  Otherwise the process is repeated starting with
the third argument, unless there is only one argument left
which is then the default.

~~~lisp
(? nil
   1)   -> nil
(? nil
   1
   2)   -> 2
(? nil
   1
   2
   3)   -> 3
(? t
   1
   2)   -> 1
(? t)   -> nil
~~~

### (and +x)

Evaluates all arguments in order unless one evaluates to
NIL.  The value of the last evaluation is returned.

~~~lisp
(and 1 2 nil) -> nil
(and 1 2)     -> 2
~~~

AND will issue an error if it is passed a dotted pair.

### (or +x)

Evaluates all arguments unless one evaluates to non-NIL.
The value of the last evaluation is returned.

~~~lisp
(or 1 nil) -> 1
(or nil 2) -> 2
~~~

OR will issue an error if it is passed a dotted pair.

### (block name . body), (return x block-name), (go tag)

Evaluates the list of expressions in BODY, returning the
value of the last unless a RETURN from the block has been
initiated.  The name of the block passed to RETURN has to
match.  It is NIL, if not specified.

~~~lisp
(block foo
  'a
  (return 'b foo)
  'c) -> b
~~~

Blocks of name NIL are used for loops.  For the purpose of
just butting up expressions use T instead to make RETURNs
for name NIL drop through.

~~~lisp
(macro progn body
  $(block t ; We don't want to catch returns.
     ,@body))
~~~

BLOCK also handles jumps initiated by GO.  A jump
destination, the "tag", must be the same symbol passed to GO
unquoted.  It is an error if the tag cannot be found in any
of the parent blocks in the current function.  If no
expression follows the tag, NIL is returned.

~~~lisp
; Print "1" and "3".
(block nil
  (print 1)
  (go jump-destination)
  (print 2)
  jump-destination
  (print 3))
~~~

## Equality

| Function    | Description                          |
|-------------|--------------------------------------|
| (eq a b)    | Test if objects are the same.        |
| (eql a b)   | Test if numbers are the equal or EQ. |
| (equal a b) | Test if trees are EQL.               |

### (eq a b): Test if objects are the same.

Tests if two objects are the very same.

Numbers usually are not as they are not looked-up for reuse
like symbols.  Use EQL instead.

### (eql a b): Test if numbers are the equal or EQ.

Like EQ except for numbers: their true values are compared
using function == instead.

### (equal a b): Test if trees are EQL.

Like EQL but traversing down conses, allowing to compare
lists and trees (lists of lists).

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

All predicates except NOT and SYMOL? return their argument
instead of T when true.

TODO: Impressive example where it's advantagous.

### (not x): NIL

Returns T on NIl and NIL otherwise.

### (atom x): not a cons

Returns its argument if it's an atom, except for NIL for
which T is returned.

### (cons? x): cons

Returns its argument if it is a cons, NIL otherwise.

### (symbol? x): symbol

Returns its argument if it is an atom.  T is returned for
NIL.  And NIL is returned for conses.

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
| (symbol-name s)  | Symbol name as List of char numbers.  |
| (char-at s n)    | Char of symbol name.                  |

### (symbol l): Make symbol with name from char list.

### (= 's x): Set symbol value.

### (symbol-value s): Get symbol value.

This is what evaluation is doing with symbols.

### (symbol-name s): Get symbol name as List of character value numbers.

### (char-at s n): Char of symbol name.

## Conses

| Function     | Description                         |
|--------------|-------------------------------------|
| (car l)      | Return first value of cons or NIL.  |
| (cdr l)      | Return second value of cons or NIL. |
| (setcar c x) | Set first value of cons.            |
| (setcdr c x) | Set second value of cons.           |

A 'cons' points to two other objects, called 'car' and 'cdr'
for historical reasons.  They could also be called 'first'
and 'second', 'first' and 'rest' or 'head' and 'tail'.
However: they are just two object pointers packed together
to form a pair.  A single cons is written with a dot in the
middle which separates the two objects it contains.  It's
called a "dotted pair":

~~~lisp
(obj-a . obj-b)
~~~

### (car l)/(cdr l): Return first or second value of cons.

CAR and CDR expect a list (cons or NIL) and return the 
first or second object a cons contains.  If the argument
is NIL, CAR and CDR return NIL.

~~~lisp
(car nil)   ; -> nil
(cdr nil)   ; -> nil

(var our-cons '(a . b))
(car our-cons) ; -> a
(cdr our-cons) ; -> b
~~~~

Because lists a conses chained up via their CDRs, this
happens with conses of lists:

~~~lisp
(var our-list '(a b))
(car our-list) ; -> a
(cdr our-list) ; -> (b)
~~~

### (setcar c x)/(setcdr c x): Set first/second value of cons.

Sets the first or second value of a cons.  Passing anything
else but a cons, e.g. NIL, is an error.
The modified cons is returned otherwise.

~~~lisp
(var our-cons '(a . b))
(setcar our-cons 'new)     ; -> (new . b)
(setcdr our-cons 'values)  ; -> (new . value)
(setcdr our-cons nil)      ; -> (new)
~~~

Setting the CDR of a *compressed cons* is also an error.
See section [compressed conses](#compressed-conses) for
details.

## Images

| Function  | Description                                |
|-----------|--------------------------------------------|
| (isave s) | Save heap image.                           |
| (iload s) | Load heap image and start function ISTART. |

Compile-time option NO\_IMAGES must be undefined to use
these.  If an image file called 'image' exists in the
current directory, that is loaded instead of default Lisp
files.

## Lists

This functions are around because the interpreter needs them
internally.

| Function          | Description                         |
|-------------------|-------------------------------------|
| (length l)        | Return length of list.              |
| (@ f l)           | Run elements through function.      |
| (butlast l)       | Copy list but not its last element. |
| (last l)          | Return last cons of list.           |
| (member x l)      | Return list starting with X.        |
| (remove x l)      | Copy list except element X.         |

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

### (length l): Return length of list.

~~~lisp
(length nil)        ; -> 0
(length '(a b)))    ; -> 2
(length '(a b c)))  ; -> 3
~~~

LENTGH also counts CDRs of dotted pairs:

~~~lisp
(length '(a b . c)))  ; -> 3
~~~

### (member x l): Return cons containing X.

~~~lisp
(member 'b '(a b c)) ; -> '(b c)
~~~

MEMBER uses EQL as the predicate, so numbers will also
match.

~~~lisp
(member 2 '(1 2 3)) ; -> '(2 3)
~~~

### (remove x l): Copy list except element X.

~~~lisp
(remove 'b '(a b c)) ; '(a c)
~~~

Uses EQ as the predicate, so REMOVE-IF must be used together
with EQL to match numbers.

### (@ f l): Filter list by function

Call function F for each element in l and returns a new
list containing the return values of F.

~~~lisp
(@ ++ '(1 2 3)) ; -> (2 3 4)
~~~

Also handles dotted pairs, filtering the last atom if it is
not NIL.  This is supported because of rest argument
definitions (which are dotted pairs).

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

| Function         | Description                         |
|------------------|-------------------------------------|
| (read)           | Read expression.                    |
| (print x)        | Print expression.                   |
| (load name)      | Load and evaluate file.             |
| (open name mode) | Open file and return channel.       |
| (err)            | Return number of last error or NIL. |
| (eof)            | Tell if read reached end of file.   |
| (setin n)        | Set input channel.                  |
| (setout n)       | Set output channel.                 |
| (in)             | Read char.                          |
| (conin)          | Read char from console.             |
| (out x)          | Print char or plain symbol name.    |
| (terpri)         | Step to next line.                  |
| (fresh-line)     | Open line if not on a fresh one.    |
| (close n)        | Close a channel.                    |
| (opendir)        | Open directory and return channel.  |
| (readdir n)      | Read directory.                     |
| (closedir n)     | Close directory.                    |

| Variable | Description            |
|----------|------------------------|
| last-in  | Last input char.       |
| last-out | Last output char.      |
| fnin     | Input channel number.  |
| fnout    | Output channel number. |

### (read): Read expression.
### (print x): Print expression.

### (load pathname): Load and evaluate file.

Loads a file expression by expression, evaluating each
right away.

Returns NIL if the file could not be loaded, or T if all
of the file has been processed successfully.

This expample loads file "subseq.lisp" and returns T when
finished:

~~~lisp
(load "subseq.lisp")
~~~

If compile-time option VERBOSE\_LOAD was defined when
TUNIX Lisp was built, a message of the form

~~~lisp
(load <pathname>)
~~~

is printed before a load is attempted.

### (open pathname mode): Open file and channel.

Opens file at PATHNAME for reading or writing.  MODE must
be a symbol.  Returns the channel number or NIL.

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
### (conin): Read console char (non-blocking).
### (out x): Print char or plain symbol name.
### (terpri): Step to next line.
### (fresh-line): Open line if not on a fresh one.
### (close channel): Close a channel.

### (opendir): Open directory and return channel.

Commodore 8-bit platforms only.

Opens the current directory and returns the channel number,
or NIL if no more channels can be allocated or an error
occured.

Then behaviour when reading from a directory channel is
undefined.

See: ERR, READDIR, CLOSEDIR

### (readdir n): Read first/next directory from channel.

Commodore 8-bit platforms only.

Returns a list of the format (name size type) or NIL if an
error occured.

See: ERR, OPENDIR, CLOSEDIR

### (closedir n): Close a directory channel.

Commodore 8-bit platforms only.  Always returns NIL and
never issues an error.  If a regular channel is applied, the
behaviour is undefined.

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

Constant +bps+ contains the number of bekloppies per
second.

On CBM machines it's currently set to 50, no
matter if it's a NTSC or PAL machine.  Please contribute
some auto-detection to file "cbm-common.lisp".

On Unices +BPS+ is 1000 and counting starts with launching
TUNIX Lisp.

Function TIME return the current count of bekloppies.
It depends on the actual machine TUNIX Lisp is running on
when the counting from 0 started.

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
| (error x)       | Issue a user error.                    |
| (onerror n x x) | User-defined error handler.            |
| (ignore)        | Break and continue with LOAD or REPL.  |
| (debug)         | Raises a SIGTRAP signal for debugging. |
| (debugger)      | Invoke debugger with next instruction. |

### (quit ?x): Return from debugger REPL.
### (exit): Stop program and go to top-level REPL.
### (error x): Issue a user error.
### (onerror n x x): User-defined error handler.
### (ignore): Break and continue with LOAD or REPL.
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

A macro is a special form which is replaced by the
expression it returns during 'macro expansion'.  Macros are
expanded in the REPL between READ and EVAL, if the value of
MACROEXPAND is a user-defined function.

Defined macros are kept in associative list \*MACROS\*.
Predicate MACRO? checks if a symbol is a defined macro in
that list.

Macros are defined with special form MACRO:

~~~lisp
; Evalue list of expressions BODY and return the result
; of the last.
(macro progn body
  $(block t
     ,@body))
~~~

Macros can also be used inside other macros, so contructs of
increasing complexity can be made from simpler components.

~~~lisp
; Evalute BODY if CONDITION is true.
(macro when (condition . body)
  $(? ,condition
      (progn
        ,@body)))

## (macro s a +x)): Define macro.

Special form, adding macro S with arguments A and body B to
\*MACROS\*.

## (macro? x): Test if symbol is in \*macros\*.

Predicate to test if a symbol denotes a macro in \*MACROS\*.

## (macroexpand x): Expand expression.

# Environment

The environment contains a widely accepted set of functions
and macros known from most other implementations of the Lisp
programming languages.

## Local variables

| Macro           | Desscription                        |
|--------------------|-------------------------------------|
| (let n init +b)    | Block with one local variable.
| (with inits +b)    | Block with many local variables.

### (let n init +b): Block with one local variable.

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
| (while (cond x) +b)  | Loop while condiiton is true.    |
| (dolist (i init) +b) | Loop over elements of a list.    |

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

Evaluates conditionally by matching values in pairs.
EQL is used as the matching predicate.

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

This is the macro-expanded version (TMP would be an
anonymous symbol):

~~~lisp
(let tmp x
  (eql tmp 'a) (out "X is A.")
  (eql tmp 5)  (out "X is 5.")
  (out "X is neither A or 5."))
~~~

### (unless cond +b): Evaluate if condition is false.

### (while (cond x) +b): Loop while condiiton is true.

### (dolist (i init) +b): Loop over elements of a list.

## Lists

| Function          | Description                         |
|-------------------|-------------------------------------|
| (list +x)         | Return list evaluated.              |
| (list? x)         | Test if argument is NIL or a cons.  |
| (cadr l)...       | Nested CAR/CDR combinations.        |
| (carlist l)       | Get first elements of lists.        |
| (cdrlist l)       | Get rest elements of lists.         |
| (copy-list x)     | Copy list.                          |
| (copy-tree x)     | Copy recursively.                   |
| (ensure-list x)   | Turn atom into list.                |
| (every f x)       | Test if F is T for all X.           |
| (some f x)        | Test if F is T for some X.          |
| (find x l)        | Find element X in list.             |
| (find-if f l)     | Find element X in list by F.        |
| (group l n)       | Split L in lists of length N.       |
| (nth n l)         | Get Nth cons in list.               |
| (nthcdr n l)      | Get Nth CDR of cons in list.        |
| (mapcar f +l)     | Map CARs of lists.                  |
| (mapcan f +l)     | Concatenating MAPCAR.               |
| (member-if f l)   | Find cons with element in list.     |
| (remove-if f l)   | Removed elemnts from list.          |
| (reverse l)       | Reverse list.                       |
| (position x l ?f) | Find position of object in list.    |
| (split x l ?f)    | Split list where object occurs.     |
| (subseq l n n)    | Return sublist.                     |

### (list +x): Make list of arguments.

### (list? x): Test if argument is NIL or a cons.

### (cadr l)...: Nested CAR/CDR combinations.

### (carlist l): Get first elements of lists.

### (cdrlist l): Get rest elements of lists.

### (copy-list x): Copy list.

### (copy x): Copy recursively.

### (find x l): Find element X in list.

### (position x l ?f) | Find position of X in L.

Returns the position of X in list L, starting with 0 for
the first position, or NIL if X has not been found.
Predicate F is EQL by default.

~~~lisp
(position 'foreign '(mom dad)) ; -> nil
(position 'mom '(mom dad))     ; -> 0
(position 'dad '(mom dad))     ; -> 1
~~~

### (split x l ?f) | Split list at object.

Splits list L where object X occurs, tested by predicate F,
which is EQL by default.  The object is removed from the
list.

~~~lisp
(split 'b '(a a b c c c)) ; -> ((a a) (c c c))
(split 'b '(a a b b c c)) ; -> ((a a) nil (c c))
~~~

## Loops

| Macro                         | Description              |
|-------------------------------|--------------------------|
| (do ((iters) (cond res)) . b) | Generic loop.            |
| (dolist (iter init) . body)   | Loop over list elements. |
| (dotimes (iter n) . body)     | Loop N times.            |

### (dolist (iter init) . body): Loop over list elements.

### (dotimes (iter n) . body): Loop N times.

## Stacks

| Macro      | Description                      |
|------------|----------------------------------|
| (push x l) | Destructively push onto stack L. |
| (pop l)    | Destructively pop from stack L.  |

Stacks are lists to which elements are pushed to or popped
off the front.

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

| Function        | Description                        |
|-----------------|------------------------------------|
| (acons alist c) | Add key/value to associative list. |
| (assoc x l)     | Return list that start with X.     |

A list of lists where the first element of each list is the
key and the rest is the value.

### (acons alist c): Add key/value to associative list.

### (assoc x l): Return list that start with X.

## Other

| Function   | Description               |
|------------|---------------------------|
| (source s) | Get definition of symbol. |

## Target information

Constant +TARGET+ identifies the target machine, which is
one of:

* c128
* c16
* c64
* pet
* plus4
* unix
* vic20

# Optional features

## Compressed conses

When enabled by compile-time option COMPRESSED\_CONS,
storing the CDR of a cons can be spared if that is following
immediately on the heap.  Since that makes compressed conses
immutable (you cannot use SETCDR on them), compression is
performed if the garbage collector was called by the program
and not the allocator.  The GC is also called to compress
conses if the available heap left is smaller than the number
of bytes specified by compile-time option
GC\_AFTER\_LOAD\_THRESHOLD, which is 2048 by default.

If you want to make sure that all conses that can be are
compressed you have to call the garbage collector twice.

Compile-time option VERBOSE\_COMPRESSED\_CONS is set, the
GC will print a 'C' to the currently active output channel.

# XXX

EXIT\_FAILURE\_ON\_ERROR

# Internals

## Heap object layouts

You can get the memory address of any object with RAWPTR.
You can then use it to PEEK and POKE memory directly.

On 32-bit and 64-bit architecture pointers and numbers are
four or eight bytes in size.  The following tables show the
layouts for 16-bit systems.

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

Adding the extended type bit turn a symbol to a special
form.  Arguments its function won't be evaluated then.

### Built-in functions

Built-ins are symbols with a pointer to a descriptor of the
built-in.  It contains an ASCIIZ pointer to the name,
another to the character-based argument definition and the
address of its implementation.

| Offset | Description                         |
|--------|-------------------------------------|
|  0     | Type info (value 8)                 |
|  1-2   | Symbol value                        |
|  3-4   | Pointer to next symbol for look-ups |
|  5     | Name length (0-255)                 |
| (6-x)  | Name (optional)                     |
