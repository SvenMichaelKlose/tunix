---
title: "TUNIX Lisp compiler"
subtitle: "A roadmap"
author: "Sven Michael Klose"
lang: "en"
titlepage: true
titlepage-color: "389fff"
titlepage-text-color: "ffffff"
toc: true
footnodes-pretty: true
book: true
...

# Why?

The TUNIX Lisp interpreter is far too slow on 6502-based systems to do anything
useful with.  By compiling to bytecode the overhead of interpretation vanishes
and the size of functions is much smaller.

# Architecture

The compiler has a _micropass_ architecture where each pass is as small and
independent from the others as possible, making the compiler easier to maintain.
Its _front end_ translates the code into a simpler, machine-level (but
still machine-independent) _intermediate representation (IR)_, where every
function is a flat list of instructions and (conditional) jumps.
That format is easier to handle with algorithms of the _middle end_, which
optimizes the IR.  The _back end_ does target-specific adjustments and generates
the desired code.

The front end also builds up a database of functions, so calls to them can be
optimized, e.g. the interpreter would have to check arguments every time a
function call is performed – the compiler does that in advance if the called
function is in that database.  Calling unknown or interpreted functions,
or using EVAL, always comes with the heavy overhead of interpretation.

## Frontend

* Macro expansion
* Compiler macro expansion (AND, OR, ?, BLOCK, RETURN, GO, QUOTE, QUASIQUOTE)
* Inlining anonymous functions
* Argument expansion
* Expression expansion
* Block folding

## Middleend

* Call expansion
* Place expansion
* Jump optimization
* Constant elimination
* Common code elimination

## Back-end

* Code generation
* Code optimization

# Compiler macros

These are the interpreter's built-in special forms as IR-generating macros.
Four new IR expressions replace the conventional forms BLOCK, GO, RETURN, ?,
AND, plus OR:

| Metacode     | Description                           |
|--------------|---------------------------------------|
| (%go s)      | Unconditional jump.                   |
| (%go-nil s)  | Jump if last result is NIL.           |
| (%go-nnil s) | Jump if last result is not NIL.       |
| (%tag s)     | Jump destination.                     |
**IR jump expressions**

QUOTEs and QUASIQUOTEs are turned into regular CONSing expressions:

~~~lisp
; Before:
'(a b c)

; After:
(cons 'a (cons 'b (cons 'c nil)))
~~~

CONSing expressions will also be made to pass rest arguments to functions,
as there is no interpreter to do it.
See [argument expansion](#argument-expansion).

## Control flow

~~~lisp
; Before:
(? (a)
   (b)
   (c)

; After:
(%= %0 (a))
(%go-nil 1)
(%= %0 (b))
(%go 2)
(%tag 1)
(%= %0 (c))
(%tag 2)
~~~

## QUASUQUOTEs

QUASIQUOTEs need to be compiled to code to apply LIST and
APPEND instead:

~~~lisp
; From:
$(1 2 ,@x 4 5)

; To:
(append '(1 2) x '(4 5))
~~~

## BLOCK expansion

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

# Function inlining

Inlines anonymous functions and moves their arguments
to the FUNINFO of the top-level function, which is laying
out the local stack frame in the process.

~~~lisp
; TODO example of anonymous function first in expression.
~~~

# Argument expansion

Checks arguments and turns rest arguments into consing expressions.  This is
the last chance to do it before expresison expansion with turn everything into
IR format for good.

# Expression expansion

The exit point of the _front end_.  Breaks up nested function calls into a list
of single statement assignments to new temporary variables.

~~~
(fun1 arg1 (fun2 (fun3) (fun4)) (fun5))

(= 2 (fun3))
(= 3 (fun4))
(= 1 (fun2 2 3)
(= 4 (fun5))
(fun1 arg1 1 4)
~~~

# Block folding

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

; (Other passes.)

; After BLOCK-FOLD.
(%= %0 (do-thing? x))
(%go-nil 1)
(do-this)
(%= %0 (do-that))
(%tag 1)
~~~

# Optimization

Basic compression of what the macro expansions messed up
at least,  like remove assignments with no effect or chained
jumps.

# Call stack expansion

This pass inserts stack place assignments of arguments before function calls.

# Place expansion

| Expression  | Description                    |
|-------------|--------------------------------|
| (%S offset) | Offset into local stack frame. |
| (%D offset) | Offset into function data.     |

Here the arguments are replaced by %S or %O expressions to
denote places on the stack or on the function's object list.

# Generating code

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

## Compiling an interpreter

The C version of the interpreter is a huge mess with lots of manual overhead
implemented to handle object pointers and the stacks.  Most getters and setters
hide behind nested macros.  It's a pain to maintain.
Generating the interpreter from Lisp code by introducing hardware types is the
remedy, hiding all that from the stressed developer.  It also makes new optimizations
possible.

~~~lisp
(defcode + a ((char * a) (char * b))
  lda a
  clc
  adc b
~~~

~~~lisp
(defcode cdr ax ((ptr a))
  ldy #3
  lda (a),y
  tax
  dey
  lda (a),y)
~~~

~~~lisp
(defcode list_cdr ax ((ptr a))
  lda (++ a)
  bne +l
  tax
  beq +l2
l:(cdr ax a)
l2:)
~~~

~~~asm
member:
    ldy #0
    lda (arg2),y
    lsr
    bcs done    ; Atom...
    ldy #cons_car+1
    lda (arg2),y
    cmp arg1+1
    bne next
    tax
    dey
    lda (arg2),y
    cmp arg1
    bne next
    lda arg2
    ldx arg2+1
    rts
next:
    ldy #cons_cdr+1
    lda (arg2),y
    tax
    dey
    lda (arg2),y
    sta arg2
    stx arg2+1
    jmp member
done:
    lda #<nil
    ldx #>nil
    rts
~~~

~~~asm
    lda v
    ldx v+1
    jsr pushax
    jsr make_cons
    lda #4
    jsr addsp
    jsr pushax
    jsr print
    jsr incsp
~~~
