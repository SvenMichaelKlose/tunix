alloc_bank:
    jsr take_over

    txa
    pha
    tya
    pha
    lda $9ff4
    pha

    lda #0
    sta $9ff4

    ; Find something free in the bitmap of allocated banks.
mod_max_banks:
    ldx #@(-- (/ 1024 8 8))
l:  lda banks,x
    cmp #$ff
    bne found_bank
    dex
    cpx #$ff
    bne -l

    sec

    pla
    sta $9ff4

return:
    pla
    tay
    pla
    tax
    jmp release

    ; Calculate number of bank for bit 0.
found_bank:
    pha
    txa
    pha
    asl
    asl
    asl
    sta tmp
    pla
    tax
    pla

    ; Find free bank (unset bit).
    ldy #0
l:  lsr
    bcc +n
    inc tmp
    iny
    jmp -l

    ; Mark bank as allocated.
n:  lda bits,y
    ora banks,x
    sta banks,x

    pla
    sta $9ff4
    lda bits,y
    ora banks,x
    sta banks,x

    clc
    jmp -return

free_bank:
    jsr take_over

    sta tmp

    lda $9ff4
    pha

    lda tmp
    and #%00000111
    tay

    lda tmp
    lsr
    lsr
    lsr
    tax

    lda banks,x
    and bits,y
    beq err_not_allocated

    lda banks,x
    and bitmasks,y
    sta banks,x

    lda $9ff4
    pha

    lda #0
    sta $9ff4

    lda banks,x
    and bitmasks,y
    sta banks,x

    pla
    sta $9ff4

    clc
    jmp release

err_not_allocated:
    pla
    sta $9ff4
    sec
    jmp release

free_process_banks:
    ldy $9ff4
    ldx @(++ mod_max_banks)
l:  lda banks,x
    eor #$ff

    pha
    lda #0
    sta $9ff4
    pla

    and banks,x
    sta banks,x

    sty $9ff4

    dex
    bpl -l

    rts

bits:   1 2 4 8 16 32 64 128

bitmasks:
    %11111110
    %11111101
    %11111011
    %11110111
    %11101111
    %11011111
    %10111111
    %01111111
