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
