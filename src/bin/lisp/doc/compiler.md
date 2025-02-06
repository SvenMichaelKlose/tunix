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
useful with.  Compiling to bytecode removes the overhead of interpretation
and reduces the code size.  Essentially, the TUNIX Lisp compiler is the smaller
sister of the [tré compiler](https://github.com/SvenMichaelKlose/tre/), minus
the overhead for generating high-level code.

# Architecture

The compiler has a _micropass_ architecture where each of the 18 passes is as
small and independent from the others as possible, making the compiler easier
to maintain.  Its _frontend_ translates the input into a simpler, machine-level
(but still machine-independent) _intermediate representation (IR)_ for the
_middleend_, where every function is a flat list of statements, assignments and
(conditional) jumps.  That IR is then optimized by the middleend.  The
_backend_ finally does target-specific adjustments and generates the desired
output, which is serializable bytecode functions.

## Frontend

1. **Dot expansion**: Dot notation for CAR, CDR and SLOT-VALUE.
2. **Unquote expansion**: Top-level unquotes to generate code at compile-time.
3. **Macro expansion**: Standard macro expansion.
4. **Compiler macro expansion**: Breaks down AND, OR, ?, BLOCK, RETURN, GO,
   QUOTE, QUASIQUOTE, and so on, into IR.   After this pass there must be no
   more macros.
5. **Quote expansion**: Makes consing expressions out of QUOTEs and QUASIQUOTEs.
6. **Lambda expansion**: Inlines binding lamdas and exports closures to own
   functions while building a function info tree required to proceed.

When compiling multiple files, all must have been processed up to the previous
pass, so all calls to known functions can be compiled in the next:

7. **Call expasions**: Arguments are expanded and rest arguments are turned into
   consing expressions.
8. **Expression expansion**: To _single statement assignments_: All arguments of
   function calls are assigned to temporary variables.
9. **Block folding**: Collapses nested %BLOCK expressions.

## Middleend

1. **Jump optimization**: Remove chained jumps, unnecessary and unused tags,
   and unreachable code.
2. **Constant elimination**: Do calculations at compile time.
3. **Common code elimination**: Remove double calculations.
4. **Unused place elimination**
5. **Peephole optimization**: Made of child passes.
6. **Tailcall optimization**: Make jumps to start of function instead of
   having it call itself if possible.

## Back-end

Translate the IR to code.

1. **Wrapping tags**: Wrap tags (numbers) in %TAG expressions for code
   generation.
1. **Place expansion**: Variables are mapped to the lexical scope.
2. **Place assignment**: Stack frames and environment vectors are laid out.
3. **Code generation**: Macros generating bytecode expressions

# Compiler macros

Expands special forms BLOCK, GO, RETURN, ?, AND, and OR, to simpler IR statements
and turn QUASIQUOTE expressions, like "$expr", into CONSing ones.

## Control flow

| Metacode     | Description                           |
|--------------|---------------------------------------|
| (%go s)      | Unconditional jump.                   |
| (%go-nil s)  | Jump if last result is NIL.           |
| (%go-nnil s) | Jump if last result is not NIL.       |
| (%tag s)     | Jump destination.                     |
**IR statements for flow control**

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

## BLOCK expansion

Different from "block folding" later.  The BLOCK expander need to expand child
blocks first so that deeper RETURNs with a clashing name have precedence:

~~~lisp
(block nil
  ...
  (block nil
    ...
    (return nil)  ; Must return from the closer BLOCK NIL.
    ...))
~~~

By translating deeper BLOCKs' first, RETURN statements are resolved in the
correct bottom-up order.

The last expression of a BLOCK always assigns to %O which is synonymous for
return values from then on.


# Quote expansion

QUOTEs are turned into regular CONSing expressions:

~~~lisp
; Before:
'(a b c)

; After:
(. 'a (. 'b (. 'c nil)))
~~~

QUASIQUOTEs are translated to code using LIST and APPEND instead:

~~~lisp
; From:
$(1 2 ,@x ,y 4 5)

; To:
(append (.. 1 2) x (. y (.. 4 5)))
~~~


# Lambda expansion

Inlines binding lambdas and turns closures into regular functions with an
environment argument for lexical scope while also building a tree of FUNINFO
objects that describe each function.  FUNINFOs are looked up by function name,
so closures are baptized with uniquely generated names.  The FUNINFOs are
essential to process functions further.

~~~lisp
; TODO example of anonymous function first in expression.
~~~


# Call expansion

Checks and expands arguments and turns rest arguments into CONSing expressions.


# Expression expansion

**The exit point of the front end.**  Breaks up nested function calls into a list
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

Nested %BLOCK expressions are collapsed and the remaining %BLOCKs are dissolved
into their function bodies – gone.

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

Basic compression of what the macro expansions messed up at least,  like
removing assignments with no effect or chained jumps.

# Call stack expansion

This pass inserts stack place assignments of arguments before function calls.

# Place expansion

| Expression  | Description                    |
|-------------|--------------------------------|
| (%S offset) | Offset into local stack frame. |
| (%D offset) | Offset into function data.     |

Here the arguments are replaced by %S or %O expressions to denote places on the
stack or on the function's object list.

# Generating code

Five byte-sized codes tell what kind of information follows them; Bytecode
BC\_LIST for example introduces a set of expressions.  The other codes are
jumps or mark the end of a function.

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

Creates function info objects with argument definitions.  They are used by
optimizing and code generating passes and, intially, are as simple as this:

~~~lisp
(fn funinfo ()
  (@ list '(args)))
~~~

When extending the compiler, most things will revolve around this pittoresque,
little thing as we'll see later.

### Argument renaming pass

The following lambda-expansion might need to inline functions with argument
names that are already in use.  This pass solves that issue by renaming all
arguments.

~~~lisp
; TODO example of shadowed arguments that would clash on
; a single list for all arguments of all functions in
; an expression.
~~~


# Code generation

* Bytecode requires functions to run in.
* These bytecode functions contain a string of byteocde in their (symbol) name,
  and bytecode only.
* Lisp objects are referenced by index in the function's object table.
* Jump destinations are also byte indexes into the bytecode string.

Bytecode functions must be PRINT- and READ-able.

~~~lisp
#$((a b)                  ; Argument definition
   (print "Hello world!") ; Object list
   nil                    ; Stack frame size or NIL
   (1 130 0))             ; Bytecodes
~~~

* Argument list elements either reference stack places or read-only object
  indexes.  The lowest bit determines what it is.  The highest bit marks the
  last argument.

~~~
(fn ir-funcall-to-bytecode (x.)
  (list
    (+ (? .x 0 128)
       (? (%stack? x.)
          (<< (cadr x.) 2)
          (++ (<< (*fi*.obj-pos x.) 2)))))
~~~

* 0:       Return from function
* 1-32:    Function call
* 128-252: Copy to stack
* 253:     Jump if not NIL
* 254:     Jump if NIL
* 255:     Jump

A set of code generating macros could be used to generate strings of assembly
language.  But all we need to do now is to comb out literal objects which must be
referenced in the bytecode function's object table.

# Futuristic ideas

## Generating native code

Native code brings maximum performance but also maximum code size, so it must
be used if top performance is essential.  For most people it's no problem if a
text editor takes a 10th of a second to respond.  But whenever huge amounts of
data have to be processed, or the system has to respond instantly, native code
is a natural choice over bytecode.

## Generating code for the MOS 6502

The amount of code required to do pointer manipulations on a MOS 6502-CPU is
massive as each byte of a pointer has to be dealt with separately.

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
