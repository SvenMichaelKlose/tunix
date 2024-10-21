TUNIX Lisp compiler
===================

# Overview

TUNIX Lisp is too slow to do anything useful with on small machines, starting with
the text editor, which is essential for developing on the system.  To take away the
overhead of interpretation the compiler translates function expressions into bytecodes
for a virtual CPU, which is an interpreter for machine-level instructions.  Compiled
functions are also smaller as every Lisp cell occupies five bytes on 8-bit machines
whereas a bytecode function call is a byte for each item of the original expression
(which is an index into the function's object list), leading to a dramatic performance
boost of all of the system.  Lots of other reasons why that is not yet mentioned.

## Passes

The compiler is translating the input to the desired output in small steps, called
"passes".  We don't care how many passes we need, as long as they are as simple and
independent from each other as possible.  That's called a "micropass architecture".
Of course we'll pass on data from one pass to the other in the form of Lisp expressions.
Some passes translate the input, some gather information or perform assertions, and some
clean up after the other passes.

Instead of directly mapping the input to code the compiler first translates the input
to a simpler, machine-level (but machine-independent) "intermediate representation (IR)",
not intended for human use, which is easier to handle with algorithms.  Imagine you want to
check if a variable is used under certain conditions somewhere in nested BLOCKs: that's
not gonna happen with the IR as every function is just a flat list of instructions and
jumps.  Bringing the code into that state is the task of the "front end".

### Frontend

* Macro expansion
* Compiler macro expansion (AND, OR, ?, BLOCK, RETURN, GO, QUOTE, QUASIQUOTE)
* Inlining anonymous functions
* Argument expansion
* Expression expansion
* Block folding

### Middleend

* Call expansion
* Place expansion

#### Optimization

* Jump optimization
* Constant elimination
* Common code elimination

### Back-end

* Code generation
* Code optimization

### Compiler macros

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

### Function inlining

Inlines anonymous functions and moves their arguments
to the FUNINFO of the top-level function, which is laying
out the local stack frame in the process.

~~~lisp
; TODO example of anonymous function first in expression.
~~~

### Argument expansion

Checks arguments and turns rest arguments into consing
expressions.

### Expression expansion

Breaks up nested function calls into a list of single
statement assignments to new temporary variables.

~~~
(fun1 arg1 (fun2 (fun3) (fun4)) (fun5))

(= 2 (fun3))
(= 3 (fun4))
(= 1 (fun2 2 3)
(= 4 (fun5))
(fun1 arg1 1 4)
~~~

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

; (Other passes.)

; After BLOCK-FOLD.
(%= %0 (do-thing? x))
(%go-nil 1)
(do-this)
(%= %0 (do-that))
(%tag 1)
~~~

### Optimization

Basic compression of what the macro expansions messed up
at least,  like remove assignments with no effect or chained
jumps.

### Call stack expansion

This pass inserts stack place assignments of arguments before function calls.

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
