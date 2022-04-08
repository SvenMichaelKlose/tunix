.import _term_init, _term_put, _term_puts, _term_get
.import _ultimem_unhide, _ultimem_is_installed

.importzp s, d, c

    .zeropage

a0:    .res 3  ; Arguments for built-in functions.
a1:    .res 3
a2:    .res 3
r:     .res 3  ; Return value of built-in function.

tmp:   .res 3
tmp2:  .res 3


    .code

    jmp _main


; ##############
; ### ERRORS ###
; ##############

    .code

.proc err_not_a_cons
    lda #<txt_err_not_a_cons
    ldx #>txt_err_not_a_cons
    jsr _term_puts
    jmp loop
.endproc

.proc err_fun_expected
.endproc

.proc err_too_many_args
.endproc

    .rodata

txt_err_not_a_cons:
    .byte "Object is not a cons: ", 0


; #####################
; ### MEMORY LAYOUT ###
; #####################
;
; $0000-$008f: Lisp zero page
; $0090-$99ff: KERNAL zero page
; $0100-$01ff: CPU stack
; $0200-$03ff: Lisp & KERNAL memory
; $0400-$0fff: Standard C lirary heap
; $1000-$1fff: Screen
; $2000-$3fff: BLK1: Lisp interpreter
; $4000-$5fff: BLK2: Lisp interpreter
; $6000-$7fff: BLK3: Lisp heap/stack window
; $9800-$9fff: Kept free for KERNAL extensions
; $a000-$bfff: Standard C lirary heap
;
; The heap is growing without gaps and accessed through
; BLK3.  An object address also comes with its bank
; number, so each pointer is three bytes in size.
;
; Every object starts with a type byte and a relocation
; address which is only used for garbage collection.
;
; The MSB of the type byte distinguishes atoms (set to 1)
; and conses (unset).

F_ATOM          = %10000000
F_SYMBOL        = %10000000
F_NUMBER        = %10000001
M_TYPE          = %00000001

F_MARKED        = %01000000
F_MISSING_TRACE = %00100000

    .zeropage

heap:       .res 3
stack:      .res 3

    .code

.proc swap_stack_and_heap
    lda stack
    ldx heap
    sta heap
    stx stack
    lda stack+1
    ldx heap+1
    sta heap+1
    stx stack+1
    lda stack+2
    ldx heap+2
    sta heap+2
    stx stack+2
    rts
.endproc

; Allocate chunk of heap.
;
; A: Type info
; X: Size
.proc alloc_heap
    sta tmp2        ; Save type.

    lda heap+1
    cmp #7          ; Last page of heap bank?
    beq check       ; Check low pointer…

    ; Save pointer to object which this function
    ; returns.
l:  lda heap
    sta r
    lda heap+1
    sta r+1
    lda heap+2
    sta r+2

    ; Save type byte.
    ldy #0
    lda tmp2
    sta (heap),y

    ; Step to next free byte for next allocation.
    txa
    clc
    adc heap
    sta heap
    lda heap+1
    adc #0
    sta heap+1

    rts

check:
    txa
    clc
    adc heap
    bcc l

.ifdef GC
    ; Mark end of heap for GC.
    ldy #0
    tya
    sta (heap),y
.endif

    ; Switch to next free bank.
    lda #$00
    sta heap
    lda #$60
    sta heap+1
    inc heap+2
    lda heap+2
    sta $9ffc

    bne l       ; (jmp)
.endproc


; ###############
; ### OBJECTS ###
; ###############
;
; Pointers point into BLK3 and are followed by the number
; of the bank BLK3 should contain.   A bank number of 0
; represents symbol NIL whereas a negative block number
; represents T.

; Built-in (EQ a b)
.proc eq
    lda #0
    sta r+2

    lda a0
    cmp a1
    bne n
    lda a0+1
    cmp a1+1
    bne n
    lda a0+2
    cmp a1+2
    bne n
    rts

n:  dec r+2
    rts
.endproc


; #############
; ### ATOMS ###
; #############

    .code

; A: Type
; X: Size
.proc alloc_atom
    pha
    jsr alloc_heap

    ldy #0
    pla
    sta (r),y
    txa
    sta (r),y

    rts
.endproc

; Built-in (ATOM x)
.proc atom
    ldy #0
    sty r+2

    lda (a0),y
    bpl n
    dec r+2
n:  rts
.endproc

; Built-in (NOT x)
.proc not
    lda #0
    sta r+2

    lda a0+2
    bne n
    dec r+2
n:  rts
.endproc


; ##############
; ### CONSES ###
; ##############

; Conses consist of a flag byte, a relocation address
; and two object pointers.

.ifdef GC
    OFS_CONS_RELOC      = 1
    OFS_CONS_A          = 3
    OFS_CONS_D          = 6
    CONS_SIZE           = 9
.else
    OFS_CONS_A          = 1
    OFS_CONS_D          = 4
    CONS_SIZE           = 7
.endif

    .code

.proc alloc_cons
    lda #0
    ldx #CONS_SIZE
    jsr alloc_heap

    ldy #OFS_CONS_A
    lda a0
    sta (r),y
    iny
    lda a0+1
    sta (r),y
    iny
    lda a0+2
    sta (r),y
    iny
    lda a1
    sta (r),y
    iny
    lda a1+1
    sta (r),y
    iny
    lda a1+2
    sta (r),y

    rts
.endproc

; Built-in (CONSP x)
.proc consp
    ldy #0
    sty r+2

    lda (a0),y
    bmi n
    dec r+2
n:  rts
.endproc

; Built-in (CAR x)
.proc car
    ldy #0
    lda (a0),y
    bmi e

    lda a0+2
    sta $9ffc

    ldy #OFS_CONS_A
    stx r
    iny
    lda (a0),y
    sta r+1
    iny
    lda (a0),y
    sta r+2

    rts

e:  jmp err_not_a_cons
.endproc

; Built-in (CDR x)
.proc cdr
    ldy #0
    lda (a0),y
    bmi e

    lda a0+2
    sta $9ffc

    ldy #OFS_CONS_D
    stx r
    iny
    lda (a0),y
    sta r+1
    iny
    lda (a0),y
    sta r+2

    rts

e:  jmp err_not_a_cons
.endproc

; Built-in (RPLACA v x)
.proc rplaca
    ldy #0
    lda (a1),y
    bmi e

    lda a1+2
    sta $9ffc

    ldy #OFS_CONS_A
    lda a0
    sta (a1),y
    sta r
    iny
    lda a0+1
    sta (a1),y
    sta r+1
    iny
    lda a0+2
    sta (a1),y
    sta r+2

    rts

e:  jmp err_not_a_cons
.endproc

; Built-in (RPLACD v x)
.proc rplacd
    ldy #0
    lda (a1),y
    bmi e

    lda a1+2
    sta $9ffc

    ldy #OFS_CONS_D
    lda a0
    sta (a1),y
    sta r
    iny
    lda a0+1
    sta (a1),y
    sta r+1
    iny
    lda a0+2
    sta (a1),y
    sta r+2

    rts

e:  jmp err_not_a_cons
.endproc


; ###############
; ### NUMBERS ###
; ###############

.ifdef GC
    OFS_NUMBER_FLAGS    = 0
    OFS_NUMBER_RELOC    = 2
    OFS_NUMBER          = 5
    NUMBER_SIZE         = 7
.else
    OFS_NUMBER_FLAGS    = 0
    OFS_NUMBER          = 2
    NUMBER_SIZE         = 4
.endif

    .bss

number:     .res 2

    .code

.proc make_number
    ldx #NUMBER_SIZE
    lda #F_NUMBER
    jsr alloc_atom

    ldy #OFS_NUMBER
    lda number
    sta (r),y
    iny
    lda number+1
    sta (r),y

    rts
.endproc


; ###############
; ### SYMBOLS ###
; ###############

.ifdef GC
    OFS_SYMBOL_FLAGS    = 0
    OFS_SYMBOL_RELOC    = 2
    OFS_SYMBOL_VALUE    = 4
    OFS_SYMBOL_LENGTH   = 7
    OFS_SYMBOL_NAME     = 8
    SYMBOL_SIZE         = OFS_SYMBOL_NAME + 1
.else
    OFS_SYMBOL_FLAGS    = 0
    OFS_SYMBOL_VALUE    = 2
    OFS_SYMBOL_LENGTH   = 5
    OFS_SYMBOL_NAME     = 6
    SYMBOL_SIZE         = OFS_SYMBOL_NAME + 1
.endif

    .zeropage

sp:             .res 2  ; Symbol name tree pointer
symbol_name:    .res 2  ; Symbol name string

    .bss

symbol_t:       .res 3
sp_root:        .res 3  ; Root node of symbol name tree

    .code

.proc symbol_length
    rts
.endproc

.proc find_symbol
    rts
.endproc

.proc insert_symbol
    rts
.endproc

.proc add_symbol
    jsr symbol_length
    clc
    adc #SYMBOL_SIZE
    tax
    lda #F_SYMBOL
    jsr alloc_atom

    ; Copy symbol name.
    lda r
    clc
    adc #SYMBOL_SIZE
    sta d
    lda r+1
    adc #0
    sta d+1
    ldy #0
l:  lda symbol_name,y
    sta (d),y
    beq n
    iny
    jmp l

n:  jmp insert_symbol
.endproc

.proc make_symbol
    stx symbol_name
    sta symbol_name+1
    jsr find_symbol
    bcs r
    jsr add_symbol
r:  rts
.endproc


; ############
; ### READ ###
; ############


; #############
; ### APPLY ###
; #############

    .zeropage

fun:    .res 3
argdef: .res 3
argsym: .res 3
body:   .res 3

    .code

.proc apply
    ; Check if function argument is a list.
    ldy #0
    lda (a0),y
    cmp #F_SYMBOL
    bne e1

    ; Push body onto stack.
    ldy #0
    lda body
    sta (stack),y
    iny
    lda body+1
    sta (stack),y
    iny
    lda body+2
    sta (stack),y

    ; Push NIL onto stack.
    lda #0
    iny
    sta (stack),y
    iny
    sta (stack),y
    iny
    sta (stack),y

    ; Advance stack pointer.
    lda stack
    clc
    adc #12
    sta stack
    lda stack+1
    adc #0
    sta stack+1

    ; Fetch pointer to argdef and body.
    lda a0+2
    sta $9ffc
    ldy #OFS_CONS_A     ; Fetch argdef.
    lda (a0),y
    sta argdef
    iny
    lda (a0),y
    sta argdef+1
    iny
    lda (a0),y
    sta argdef+2
    iny
    lda (a0),y
    sta body
    iny
    lda (a0),y
    sta body+1
    iny
    lda (a0),y
    sta body+2

n1: lda a1+2
    bne l2
    jmp end_of_args

e1: jmp err_fun_expected
e2: jmp err_too_many_args

    ; Get symbol from argument definition.
l2: lda argdef+2
    beq e2
    sta $9ffc
    ldy #OFS_CONS_A
    lda (argdef),y
    sta argsym
    iny
    lda (argdef),y
    sta argsym+1
    iny
    lda (argdef),y
    sta argsym+2

    ; Push symbol onto stack.
    ldy #0
    lda argsym
    sta (stack),y
    iny
    lda argsym+1
    sta (stack),y
    iny
    lda argsym+2
    sta (stack),y

    ; Push symbol value onto stack.
    ldy #OFS_SYMBOL_VALUE
    lda (argsym),y
    sta tmp
    iny
    lda (argsym),y
    tax
    iny
    lda (argsym),y
    ldy #6
    sta (stack),y
    dey
    txa
    sta (stack),y
    dey
    lda tmp
    sta (stack),y

    ; Advance stack pointer.
    lda stack
    clc
    adc #12
    sta stack
    lda stack+1
    adc #0
    sta stack+1

    ; Fetch value from argument list.
    lda a1+2
    sta $9ffc
    ldy #OFS_CONS_A
    lda (a1),y
    sta tmp
    iny
    lda (a1),y
    sta tmp+1
    iny
    lda (a1),y
    sta tmp+2

    ; Fetch pointer to next argument.
    iny
    lda (a1),y
    sta tmp2
    iny
    lda (a1),y
    sta tmp2+1
    iny
    lda (a1),y
    sta tmp2+2

    ; Set new symbol value.
    lda argsym+2
    sta $9ffc
    ldy #OFS_SYMBOL_VALUE
    lda tmp
    sta (argsym),y
    iny
    lda tmp+1
    sta (argsym),y
    iny
    lda tmp+2
    sta (argsym),y

    ; Step to next argument.
    lda tmp2
    sta a1
    lda tmp2+1
    sta a1+1
    lda tmp2+2
    sta a1+2

    jmp n1

end_of_args:
    lda argdef+2
    beq l3
    jmp err_too_few_args

    ; Call EVAL-BODY.
l3:

    ; Undo stack pointer.
n2: lda stack
    sec
    sbc #3
    sta stack
    lda stack+1
    sbc #0
    sta stack+1

    ; Pop symbol.
    ldy #0
    lda (stack),y
    sta argsym
    iny
    lda (stack),y
    sta argsym+1
    iny
    lda (stack),y
    beq done        ; Store pointer on stack in reverse
                    ; for this test to come first.
    sta argsym+2
    sta $9ffc

    ; Undo stack pointer.
    lda stack
    sec
    sbc #3
    sta stack
    lda stack+1
    sbc #0
    sta stack+1

    ; Pop symbol value.
    ldy #0
    lda (stack),y
    sta tmp2
    iny
    lda (stack),y
    tax
    iny
    lda (stack),y

    ; Assign old symbol value.
    ldy #OFS_SYMBOL_VALUE+2
    sta (argsym),y
    dey
    txa
    lda (argsym),y
    dey
    lda tmp2
    lda (argsym),y

    jmp n2

done:
    ; Undo stack pointer.
    lda stack
    sec
    sbc #3
    sta stack
    lda stack+1
    sbc #0
    sta stack+1

    ; Pop old exec ptr.
    ldy #0
    lda (stack),y
    sta body
    iny
    lda (stack),y
    sta body+1
    tax
    iny
    lda (stack),y
    sta body+2

    rts

err_too_few_args:
    rts

noargs:
    ; Call EVAL-BODY.
    rts

builtin:
    ; Copy arguments to zero page argument list.
    ; Call built-in.

    rts
.endproc


; #################
; ### TOP-LEVEL ###
; #################

    .code

.proc loop
    lda #<txt_prompt
    ldx #>txt_prompt
    jsr _term_puts

l:  jsr _term_get
    jsr _term_put
    jmp l
.endproc

    .rodata

txt_prompt:
    .byte "* ", 0


; ############
; ### INIT ###
; ############

    .code

.proc _main
    jsr _term_init

    lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

;    jsr _ultimem_unhide
;    jsr _ultimem_is_installed
;    tax
;    bne ultimem_found

;    lda #<txt_no_ultimem
;    ldx #>txt_no_ultimem
;    jsr _term_puts

ultimem_found:
    lda #$00
    sta heap
    lda #$60
    sta heap+1

    lda #$00
    sta stack
    lda #$40
    sta stack+1

;    lda #<txt_t
;    ldx #>txt_t
;    jsr make_symbol
;    lda r
;    sta symbol_t
;    lda r+1
;    sta symbol_t+1

    ldx #31
    jsr alloc_heap

    jmp loop
.endproc

    .rodata

txt_t:
    .byte "t",0

txt_welcome:
    .byte "AttoLisp", 10, 13
    .byte "by Sven Michael Klose <pixel@hugbox.org>", 10, 13
    .byte "Welcome!", 10, 13
    .byte 10, 13, 0

txt_no_ultimem:
    .byte "No UltiMem expansion found. Stopped.", 0
