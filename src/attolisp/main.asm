.import _term_init, _term_put, _term_puts, _get_key

.importzp s, d, c

    .zeropage

a0:         .res 3  ; Arguments to built-in functions.
a1:         .res 3
a2:         .res 3
r:          .res 3  ; Return value of built-in function.

tmp:        .res 3
tmp2:       .res 3


; ############
; ### INIT ###
; ############

    .code

.proc _main
    jsr _term_init

    lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

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

    .data

txt_t:
    .byte "t",0

txt_welcome:
    .byte "AttoLisp v0.1 2021-10-03", 13
    .byte "by Sven Michael Klose <pixel@hugbox.org>", 13
    .byte "Welcome!", 13
    .byte "* "
    .byte 0


; #################
; ### TOP-LEVEL ###
; #################

    .code

.proc loop
l:  jsr _get_key
    jsr _term_put
    jmp l
.endproc


; ############
; ### HEAP ###
; ############

; Every object starts with a flag byte, a size byte and
; the new address for relocation during garbage collection.
;
; The heap is steadily growing in BLK3.  An object address
; also comes with its bank number, so each pointer is three
; bytes in size.
; The MSB in the first object byte distinguishes atoms and
; conses.  If it is set, the object is an atom and the seven
; bits left contain flags.
; An object cannot be larger than 255 bytes.

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

.proc alloc_heap ; X: Size
    sta tmp2

    lda heap+1
    cmp #7          ; Last page of heap bank?
    beq check       ; Check low pointer...

    ; Save pointer to object.
l:  lda heap
    sta r
    lda heap+1
    sta r+1
    lda heap+2
    sta r+2

    ldy #0
    lda tmp2
    sta (heap),y

    ; Step to next free space.
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

    ; Switch to next free bank.
    inc heap+2
    lda heap+2
    sta $9ffc

    ; Reset heap pointer to start of bank.
    lda #$00
    sta heap
    lda #$60
    sta heap+1

    bne l       ; (jmp)
.endproc


; #############
; ### ATOMS ###
; #############

    .code

; X: Flags/size
.proc alloc_atom
    pha
    jsr alloc_heap

    ; Set atom flag.
    ldy #0
    pla
    sta (r),y
    txa
    sta (r),y

    rts
.endproc


; ##############
; ### CONSES ###
; ##############

; Conses consist of a flag byte, a relocation address and two object pointer.

OFS_CONS_FLAGS      = 0
OFS_CONS_RELOC      = 1
OFS_CONS_A          = 3
OFS_CONS_D          = 6
CONS_SIZE           = 9

    .code

.proc alloc_cons
    lda #CONS_SIZE
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

.proc rplaca
    ldy #0
    lda (a1),y
    bmi e

    lda a1+2
    sta $9ffc

    ldy #OFS_CONS_A
    lda a0
    sta (a1),y
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

.proc rplacd
    ldy #0
    lda (a1),y
    bmi e

    lda a1+2
    sta $9ffc

    ldy #OFS_CONS_D
    lda a0
    sta (a1),y
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
; ### SYMBOLS ###
; ###############

OFS_SYMBOL_FLAGS    = 0
OFS_SYMBOL_RELOC    = 2
OFS_SYMBOL_VALUE    = 4
OFS_SYMBOL_LENGTH   = 6
OFS_SYMBOL_NAME     = 7
SYMBOL_SIZE         = OFS_SYMBOL_NAME

    .zeropage

symbol_t:       .res 3
sp_root:        .res 3  ; Root node of symbol name tree
sp:             .res 2  ; Symbol name tree pointer
symbol_name:    .res 2  ; Symbol name string


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


; ###############
; ### NUMBERS ###
; ###############

OFS_NUMBER_FLAGS    = 0
OFS_NUMBER_RELOC    = 2
OFS_NUMBER          = 5
NUMBER_SIZE         = 7

    .zeropage

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


; ############
; ### READ ###
; ############

; #############
; ### APPLY ###
; #############

    .code

.proc apply
    lda a1
    sta tmp
    lda a1+1
    sta tmp+1
    lda a1+2
    sta tmp+2

n1: lda a1
    ora a1+1
    beq end_of_args

    ; Map in argument.
    lda a1+2
    sta $9ffc

    ; Push symbol.
    ldy #0
    lda a1
    sta (stack),y
    iny
    lda a1+1
    sta (stack),y
    iny
    lda a1+2
    sta (stack),y

    ; Push symbol value.
    ldy #OFS_SYMBOL_VALUE
    lda (a1),y
    sta tmp
    iny
    lda (a1),y
    tax
    iny
    lda (a1),y
    ldy #3
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
    adc #6
    sta stack
    lda stack+1
    adc #0
    sta stack+1

    ; Step to next argument.
    ldy #0
    lda (a1),y
    sta tmp
    lda (a1),y
    tax
    iny
    lda (a1),y
    sta a1+2
    txa
    sta a1+1
    pla
    sta a1

    jmp n1

end_of_args:
    ; Push argument list.
    ldy #0
    lda tmp
    sta (stack),y
    iny
    lda tmp+1
    sta (stack),y
    iny
    lda tmp+2
    sta (stack),y

    ; Advance stack pointer.
    lda stack
    clc
    adc #3
    sta stack
    lda stack+1
    adc #0
    sta stack+1

    ; Call EVAL-BODY.

    ; Undo stack pointer.
    lda stack
    sec
    sbc #3
    sta stack
    lda stack+1
    sbc #0
    sta stack+1

    ; Pop argument list.
    ldy #0
    lda tmp
    sta (stack),y
    iny
    lda tmp+1
    sta (stack),y
    iny
    lda tmp+2
    sta (stack),y

n2: lda tmp
    ora tmp+1
    beq done

    ; Undo stack pointer.
    lda stack
    sec
    sbc #6
    sta stack
    lda stack+1
    sbc #0
    sta stack+1

    ; Map in argument.
    lda a1+2
    sta $9ffc

    ; Pop symbol.
    ldy #0
    lda (stack),y
    sta tmp
    iny
    lda (stack),y
    sta tmp+1
    iny
    lda (stack),y
    sta tmp+2

    ; Pop symbol value.
    iny
    lda (stack),y
    sta tmp2
    iny
    lda (stack),y
    tax
    iny
    lda (stack),y
    sta tmp2+2

    ; Assign old symbol value.
    ldy #OFS_SYMBOL_VALUE+2
    sta (a1),y
    dey
    txa
    lda (a1),y
    dey
    lda tmp2
    lda (a1),y
    ldy #3

    ; Step to next argument.
    ldy #0
    lda (a1),y
    sta tmp
    lda (a1),y
    tax
    iny
    lda (a1),y
    sta a1+2
    txa
    sta a1+1
    pla
    sta a1

    jmp n2

done:
    rts

noargs:
    ; Call EVAL-BODY.
    rts

builtin:
    ; Copy arguments to zero page argument list.
    ; Call built-in.

    rts
.endproc


; ##########################
; ### GARBAGE COLLECTION ###
; ##########################

    .zeropage

last_object:    .res 3
p:              .res 3

    .code

.proc push_gc_ptr
    ldy #0
    lda p
    sta (stack),y
    iny
    lda p+1
    sta (stack),y
    iny
    lda p+2
    sta (stack),y
    iny
    lda stack
    clc
    adc #3
    sta stack
    lda stack+1
    adc #0
    sta stack+1
    rts
.endproc

.proc pop_gc_ptr
    lda stack
    sec
    sbc #3
    sta stack
    lda stack+1
    sbc #0
    sta stack+1
    ldy #0
    lda (stack),y
    sta p
    iny
    lda (stack),y
    sta p+1
    iny
    lda (stack),y
    sta p+2
    rts
.endproc

.proc mark
    lda p+2
    sta $9ffc

    ldy #0
    ldy #OFS_CONS_FLAGS
    lda (p),y
    ora #F_MARKED
    sta (p),y
    bmi mark_atom

mark_cons:
    jsr push_gc_ptr

    ; Copy A to tmp
    ldy #OFS_CONS_A
    lda (p),y
    sta tmp
    iny
    lda (p),y
    sta tmp+1
    iny
    lda (p),y
    sta tmp+2

    ; Copy tmp to p
    lda tmp
    sta p
    lda tmp+1
    sta p+1
    lda tmp+2
    sta p+2

    jsr mark

    jsr pop_gc_ptr

    ; Copy D to p
    ldy #OFS_CONS_D
    lda (p),y
    sta p
    iny
    lda (p),y
    sta p+1
    iny
    lda (p),y
    sta p+2

    lda p
    ora p+1
    bne mark

r:  rts

mark_atom:
    and #M_TYPE
    cmp #F_NUMBER
    beq r

    jsr push_gc_ptr

    ; Copy symbol value to tmp
    ldy #OFS_SYMBOL_VALUE
    lda (p),y
    sta tmp
    iny
    lda (p),y
    sta tmp+1
    iny
    lda (p),y
    sta tmp+2

    ; Copy tmp to p
    lda tmp
    sta p
    lda tmp+1
    sta p+1
    lda tmp+2
    sta p+2

    jsr mark

    jmp pop_gc_ptr
.endproc

.proc set_reloc_addr
    lda last_object
    sta (p),y
    iny
    lda last_object+1
    sta (p),y
    iny
    lda last_object+2

    lda p
    sta last_object
    lda p+1
    sta last_object+1
    lda p+2
    sta last_object+2

    rts
.endproc

.proc reloc
    lda #$00
    sta p+2

next_bank:
    lda #$01
    sta p
    lda #$60
    sta p+1

l:  lda p+2
    sta $9ffc

    lda p
    cmp heap
    bne n
    lda p+1
    cmp heap+1
    beq done

n:  ldy #0
    lda (p),y
    beq next_bank
    and #F_MARKED
    bne update

    lda (p),y
    bmi skip_atom

    lda #CONS_SIZE
    jmp next_item

skip_atom:
    and #M_TYPE
    cmp #F_NUMBER
    bne skip_symbol

number_done:
    lda #NUMBER_SIZE
    jmp next_item

skip_symbol:
    ldy #OFS_SYMBOL_LENGTH
    lda (p),y
    clc
    adc #SYMBOL_SIZE
    jmp next_item

update:
    jsr set_reloc_addr

    ldy #0
    lda (p),y
    and #M_TYPE
    cmp #F_NUMBER
    beq number_done
    bne skip_symbol

next_item:
    clc
    adc p
    sta p
    lda p+1
    adc #0
    sta p+2

    jmp l

done:
    rts
.endproc


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

    .data

txt_err_not_a_cons:
    .byte "Object is not a cons: ", 0
