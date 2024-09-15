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

Interpreted languages have become the most popular and come
with several advantages compilers cannot provide, like
changing code at run time.  They are machine-independent
most of the time and most imporantly: they make developing
programs easier.  This is brought to the extreme with
Lisp.[^picolisp]  Unfortunately, interpreted Lisp is
particularly slow on machines that don't support the pointer
manipulations that take place most of the time, and the
overhead of interpretation is weighing in heavily on
machines that tick in the one-digit MHz area or less.

[^picolisp]:
  On machines that do, PicoLisp shows that an interpreter is
  sufficient in most cases and that it comes with an overwhelming set of
  advantages. https://picolisp.com/

A compiler takes away the overhead required by interpreters
to handle any kind of code that comes in as it regards the code that will
be executed in advance.  In a completed program for example,
arguments passed to functions usually match the argument
definitions of those functions.  Still, an interpreter
always checks if the arguments match their definition to
stop the program (to avoid hazards) and issue the error.

Compile-time option NAIVE takes those checks out of TUNIX
Lisp entirely, but that is highly impractical in a
development environment: Although the integrated editor
becomes appropriately responsive, not being able to detect
mistakes on the spot is rendering the whole system useless.
Making those checks optional at run-time would only add even
more overhead, adding checks if checks have to be performed,
and not all checks can be ommitted as rest arguments can
only be handled using argument definitions when interpreted.

A compiler checks the validiy of argument lists before the
program is running and generates correct code to set them
up.  When giving up on symbols and using the stack instead
to carry arguments, simpler machine-level instructions can
be used, leading to massive performance improvement.

We'll use bytecode as machine-level code because it is
portable, much more compact than native code (which is the
alternative) and way easier to debug when writing a
compiler.  Once the bytecode target is running, generating
native code is less likely to cause trouble.  We'll refer
to bytecode and machine language as "machine-level code"
accordingly.

Machine-level code is an array of bytes, containing a sequence
of instructions that assemble function calls and jumps that
may be conditional.  We need to convert nested Lisp code to
a list of instructions, using jumps instead of the special
forms AND, OR, ?, BLOCK/RETURN/GO, and inlined functions
that introduce local variables.  Initially, function calls
won't be compiled.  We'll have the expressions interpreted,
so we can check how much was gained already.  It may also be
practical in some cases.  (That's a warm fuzzy feeling about it
only.)

Instead of dealing with machine-level code as bytes, we'll use
regular expressions that represent machine-level
instructions, called "metacode".  They are translated to real
code in the end.

### Compiler macro expansion

Expands all special forms in functions.  Control flow
special forms (BLOCK, GO, RETURN, ?, AND, OR) are translated
to machine-level jump and tag expressions.  QUASIQUOTES
are converted to regular consing expressions.

| Metacode     | Description                           |
|--------------|---------------------------------------|
| (%go s)      | Unconditional jump.                   |
| (%go-nil s)  | Jump if last result is NIL.           |
| (%go-nnil s) | Jump if last result is not NIL.       |
| (%tag s)     | Jump destination.                     |

#### Control flow

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

#### QUASUQUOTEs

QUASIQUOTEs need to be compiled to code to apply LIST and
APPEND instead:

~~~lisp
; From:
$(1 2 ,@x 4 5)

; To:
(append '(1 2) x '(4 5))
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

### Generating code

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


These are actually two passes:

* Collecting objects.
* Calculating jump destinations.

# Adding lexical scope

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


# Generating native code

Native code brings maximum performance but also maximum code size, so it must be used
if top performance is essential.  For most people it's no problem if a text editor
takes a 10th of a second to respond.  But whenever huge amounts of data have to be
processed, or the system has to respond instantly, native code is a natural choice
over bytecode.

## Generating code for the MOS 6502

The amount of code required to do pointer manipulations on a
MOS 6502-CPU is massive as each byte of a pointer has to be
dealt with separately.

~~~asm
not:ldy #0
    lda (sp),y
    beq ret_nil
    lda #<t
    sta (sp),y
    lda #>t
    iny
    sta (sp),y
    rts
ret_nil:
    lda #0
    sta (sp),y
    iny
    sta (sp),y
    rts
~~~

~~~asm
    lda sp
    sec
    sbc sp
    sta sp
    bcs n
    dec sp+1
n:  ldy #ofs_symbol_value
    lda (sym),y
    tax
    iny
    lda (sym),y
    ldy #1
    sta (sp),y
    dey
    txa
    sta (sp),y
~~~

## Generating code for the Zilog Z80

The Z80 is an 8-bit CPU.  Although it provides 16-bit register pairs,
the ALU provides 8-bit operations only, leading to the same issues the
6502 introduces.  There are 16-bit load and store instruction to make
use of.  These are so precious for compiled Lisp that it's worth trying
out tags to use the regular stack for objects.

With a separate object stack pointed to by the IX register:

~~~asm
    ; Push value onto the object stack IX.
    ld hl,sym
    inc ix
    lda (ix+0),h
    inc ix
    lda (ix+0),l
~~~

Regular stack use:

~~~asm
    ; Push value onto regular stack.
    ld hl,sym
    push hl
~~~

There are also function calls and the GC must leave the
return addresses untouched, assuming that there'll be
no temporaries on the stack either.  That can be achieved
easily if native code is limited to a particular memory area.

~~~asm
    ld iy,sym
    ld h,(iy+ofs_symbol_value)
    ld l,(iy+ofs_symbol_value+1)
    inc ix
    ld (ix+0),h
    inc ix
    ld (ix+0),l
~~~


~~~asm
not:
    ld ix,sp
    ld a,(ix+1) ; Only the high byte needs to be checked.
    cp 0
    jr z,ret_nil
    lda hl,t
    rts
    lda hl,0
    rts
~~~
