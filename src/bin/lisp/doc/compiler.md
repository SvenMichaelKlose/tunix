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

Interpreting Lisp is particularly slow on 8-bit CPUs like
the 6502-CPU.  A 16-bit pointer manipulation takes two
operations on these.  On CPUs that support 16-bit, at least
internally, like the Z80, performance doubles and code size
halves.

Interpretation also comes with a lot of overhead as the
course of a running program is not known in advance.
Arguments have to be expanded, including saving and
restoring symbol values, and lots of other checks that have
to be performed for the interpreter to function and allow
the programmer to get information about errors.

Most of that overhead goes away if the code has been
compiled.  Arguments can be expanded at compile-time and the
object stack can be used instead of symbols.  But for our
compiler we care about control flow only and evaluate
expressions without jumps the regular way.

Finally, we'll check how much has been achieved, to decide
on improvements worth their effort.  For now, we expect to
bring on enough oomfh to allow for a usable text editor.

## The simplest compiler possible

For the first version of the compiler we want at least the
control flow instructions to be compiled, so all other
expressions are on a list that isn't nested, so jumps can
specify their destinations by list offset.
Jumps and expressions are all on one level.

Five byte-sized codes tell what kind of information follows
them; Bytecode BC\_LIST for example introduces a set of expressions.
The other codes are jumps or mark the end of a function.

| Bytecode         | Description                        |
|------------------|------------------------------------|
| BC\_END          | End of bytecode function.          |
| BC\_LIST, n, ... | List of N expressions to evaluate. |
| BC\_GO, n        | Unconditional jump.                |
| BC\_GO\_NIL, n   | Jump if last result is NIL.        |
| BC\_GO\_NNIL, n  | Jump if last result is not NIL.    |

Here's what's wanted before it's assembled to bytecode:

### Compiler macro expansion

The first compiler pass.
Expands control flow special forms (BLOCK, GO, RETURN, ?,
AND, OR) to simpler jump and tag expressions:

| Metacode     | Description                           |
|--------------|---------------------------------------|
| (%go s)      | Unconditional jump.                   |
| (%go-nil s)  | Jump if last result is NIL.           |
| (%go-nnil s) | Jump if last result is not NIL.       |
| (%tag s)     | Jump destination.                     |

Jump tags must be EQ.

#### Expansion of ?, AND and OR

~~~lisp
(? (a)
   (b)
   (c)

(%= %0 (a))
(%go-nil 1)
(%= %0 (b))
(%go 2)
(%tag 1)
(%= %0 (c))
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
(%go-nil 1)
(%block
  (= %0 (do-this))
  (= %0 (do-that)))
(%tag 1)

; After BLOCK-FOLD.
(%= %0 (do-thing? x))
(%go-nil 1)
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
functions with argument names that are already in use.  This
pass solves that issue by renaming all arguments.
[^bcdbgarg]

[^bcdbgarg]: A map of the original names must be created if
    debugging must be supported.

~~~lisp
; TODO example of shadowed arguments that would clash on
; a single list for all arguments of all functions in
; an expression.
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
