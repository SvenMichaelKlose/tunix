---
title: "((())) TUNIX Lisp"
subtitle: "Bytecode Compiler Roadmap"
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

TUNIX Lisp is a highly efficient Lisp interpreter, written
in ANSI-C.  It is designed for constrained environments,
such as embedded systems, classic home computers.

This is the plan on implementing a bytecode compiler to gain
improved performance and smaller code size.  The compiler
is written in TUNIX Lisp itself.

## The target machine: Bytecode format

| Offset | Size  | Description                         |
|--------|-------|-------------------------------------|
| 0      | 1     | Object TYPE\_BYTECODE                |
| 1      | 1     | Total size - 3                      |
| 2      | 2     | Argument definition                 |
| 3      | 1     | Local stack size / object list size |
| 4      | 1     | Code offset                         |
| 5      | ?     | Data (garbage-collected objects)    |
| <5     | ?     | Raw data                            |
| <5     | 1-220 | Code                                |

A bytecode function has a memory layout similar to that of
a symbol, with a type, length and value slot.  The length
tells the number of bytes following.  The value points to a
regular argument definition list , e.g.  '(first . rest)'.
Addtionally the size of the local stack frame, which is
subtracted from the stack pointer upon function entry, is
given, followed by the length of a GCed object list and the
size of a raw data array which contains jump destination
offsets.  All that is followed by the actual bytecode.

### Instruction format

The highest bit of the first byte determines if the code is
an assignment or a jump.  Jumps also contain an index into
a code offset table.

#### Jumps

~~~
1JJIIIII
~~~

* J: type
 * 00: unconditional
 * 01: If %0 is not NIL.
 * 10: If %0 is NIL.
 * 11: unused
* I: Index into raw data table

#### Return from function

~~~
00000000
~~~

#### Assignment:

Assigments include a destination on the stack, a function
object and the arguments which are either on the object list
or on the stack..

~~~
0?DDDDDD FFFFFFFF
~~~

* D: Destination on stack
* F: Function (index into object array)

#### Arguments:

Arguments are either indexes into the object array or into
the stack.

~~~
EPIIIIII
~~~

* E: End of argument list flag
* P: 0: stack place 1: object
* I: Index into stack or object array

## Passes

The TUNIX compiler has a micro-pass architecture, requiring
only workspace for the current pass.  It translates
macro-expanded input into an assembly-style metacode made of
assignments, jumps and jump tags.  Function information is
also gathered for the following optimization and code
generation passes.

Transform to metacode:

* Compiler macro expansion
* Quote expansion
* Quasiquote expansion
* Argument renaming
* Function collection
* Scoping
* Lambda expansion
* Block folding
* Expression expansion

After that transformation the resulting code has to be
cleaned from macro artifacts.

Cleaning up at least:

* Optimization

Finally the desired code can be generated, e.g. byte code or
assembly language.

Code generation:

* Place expansion
* Code macro expansion

### Compiler macro expansion

Expands control flow special forms (BLOCK, GO, RETURN, ?,
AND, OR) to these assembly-level jump and tag expressions:

| Metacode     | Description                           |
|--------------|---------------------------------------|
| (%= d f +x)  | Call F with X and assign result to D. |
| (%jmp s)     | Unconditional jump.                   |
| (%jmp-nil s) | Jump if %0 is NIL.                    |
| (%jmp-t s)   | Jump if %0 is not NIL.                |
| (%tag s).    | Jump destination.                     |

Jump tags must be EQ.

#### Expansion of ?, AND and OR

~~~lisp
(? (a)
   (b)
   (c)

(%= %0 a)
(%jmp-nil 1)
(%= %0 b)
(%jmp 2)
(%tag 1)
(%= %0 c)
(%tag 2)
~~~

#### BLOCK expansion

The BLOCK expander need to expand child blocks first so
that deeper RETURNs with a clashing name have precedence:

~~~lisp
(block nil
  ...
  (block nil
    ...
    (return nil)  ; Must return from the closer BLOCK NIL.
    ...))
~~~

By translating deeper BLOCKs' first, RETURN statements are
resolved in the correct bottom-up order.

The last expression of a BLOCK always assigns to %O which is
synonymous for return values from then on.

### Block folding

BLOCKs have been expanded to %BLOCKs to hold the expressions
together for this pass.  They are now spliced into each
other to get a single expression list for each function.

~~~lisp
; Input code
(when (do-thing? x)
  (do-this)
  (do-that))

; After MACROEXPAND.
(? (do-thing? x)
   (block t
     (do-this)
     (do-that)))

; After COMPILER-MACROEXPAND.
(%= %0 (do-thing? x))
(%jmp-nil 1)
(%block
  (= %0 (do-this))
  (= %0 (do-that)))
(%tag 1)

; After BLOCK-FOLD.
(%= %0 (do-thing? x))
(%jmp-nil 1)
(do-this)
(%= %0 (do-that))
(%tag 1)
~~~

### Quote expansion

Quotes are entries on the function's object list.

### Quasiuote expansion

QUASIQUOTEs need to be compiled to code to apply LIST and
APPEND instead.

~~~lisp
; From:
$(1 2 ,@x 4 5)

; To:
(append '(1 2) x '(4 5))
~~~

### Function info collection

Creates function info objects with argument definitions.
They are used by optimizing and code generating passes and,
intially, are as simple as this:

~~~lisp
(fn funinfo ()
  (@ list '(args)))
~~~

When extending the compiler, most things will revolve around
this pittoresque, little thing as we'll see later.

### Argument renaming pass

The following lambda-expansion might need to inline
functions with argument names that are already in use.
This pass solves that issue by renaming all arguments.
[^bcdbgarg]

[^bcdbgarg]: A map of the original names must be created if
  debugging is in order.

~~~lisp
; TODO example of shadowed arguments that would clash on
; a common list.
~~~

### Lambda expansion

Inlines anonymous functions and moves their arguments
to the FUNINFO of the top-level function, which is laying
out the local stack frame in the process.

~~~lisp
; TODO example of anonymous function first in expression.
~~~

### Expression expansion

Breaks up nested function calls into a list of single
statement assignments.  After this the return value of
any expression is in variable %0.

This expression

~~~
(fun1 arg1 (fun2 (fun3) (fun4)) (fun5))
~~~

becomes:

~~~
(%= %1 fun3)
(%= %2 fun4)
(%= %3 fun2 %1 %2)
(%= %4 fun5)
(%= %0 fun1 %0 %3 %4)
~~~

### Argument expansion

Checks arguments and turns rest arguments into consing
expressions.

### Optimization

Basic compression of what the macro expansions messed up
at least,  like remove assignments with no effect or chained
jumps.

### Place expansion

| Expression  | Description                    |
|-------------|--------------------------------|
| (%S offset) | Offset into local stack frame. |
| (%D offset) | Offset into function data.     |

Here the arguments are replaced by %S or %O expressions to
denote places on the stack or on the function's object list.

### Code expansion

These are actually two passes:

* Collecting objects.
* Calculating jump destinations.

# Internals

## Heap object layouts

You can get the memory address of any object with RAWPTR.
You can then use it to PEEK and POKE memory directly.

On 32-bit and 64-bit architecture pointers and numbers are
four or eight bytes in size.  The following tables show the
layouts for 16-bit systems.

All objects start with a type byte:

| Bit | Description                        |
|-----|------------------------------------|
|  0  | Type bit for conses                |
|  1  | Type bit for numbers               |
|  2  | Type bit for symbols               |
|  3  | Type bit for built-in functions    |
|  4  | Extended type (for symbols)        |
|  5  | Unused                             |
|  6  | Unused                             |
|  7  | Mark bit for garbage collection    |

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
