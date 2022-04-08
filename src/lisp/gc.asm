; ##########################
; ### GARBAGE COLLECTION ###
; ##########################

.ifdef GC
    .zeropage

p:  .res 3

    .bss

last_object:    .res 3

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
    ldy #1
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
    lda #$00
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

.proc get_reloc_address
    lda tmp+2
    sta $9ffc

    ldy #1
    lda (tmp),y
    pha
    iny
    lda (tmp),y
    tax
    iny
    lda (tmp),y
    sta tmp+2
    stx tmp+1
    pla
    sta tmp

    rts
.endproc

.proc update_value
    tya
    pha

    lda p+2
    sta $9ffc

    lda (p),y
    sta tmp
    iny
    lda (p),y
    sta tmp+1
    iny
    lda (p),y
    sta tmp+2

    jsr get_reloc_address

    pla
    tay

    lda p+2
    sta $9ffc

    lda tmp
    sta (p),y
    iny
    lda tmp+1
    sta (p),y
    iny
    lda tmp+2
    sta (p),y

    rts
.endproc

.proc update_values
    lda #$00
    sta p+2

next_bank:
    lda #$00
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
    ldy #0
    lda (p),y
    bpl update_cons

    and #M_TYPE
    cmp #F_NUMBER
    beq number_done

    ldy #OFS_SYMBOL_VALUE
    jsr update_value
    jmp skip_symbol

update_cons:
    ldy #OFS_CONS_A
    jsr update_value
    ldy #OFS_CONS_D
    jsr update_value

    lda #CONS_SIZE

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

.endif ; .ifdef GC
