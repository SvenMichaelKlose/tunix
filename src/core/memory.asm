alloc_bank:
    ; Find something free in the bitmap of allocated banks.
    ldx #$ff
l:  lda banks,x
    cmp #$ff
    bne found_bank
    dex
    cpx #$ff
    bne -l
    sec
    rts

    ; Calculate number of bank for bit 0.
found_bank:
    pha
    txa
    pha
    asl
    adc #0
    asl
    adc #0
    asl
    adc #0
    tax
    and #%00000111
    sta @(++ tmp)
    txa
    and #%11111000
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
    clc
    rts

free_bank:
    lda tmp
    and #%00000111
    tay
    lsr @(++ tmp)
    ror tmp
    lsr @(++ tmp)
    ror tmp
    lsr @(++ tmp)
    ror tmp
    ldy tmp
    lda banks,x
    and bits,y
    beq err_not_allocated
    lda banks,x
    and bitmasks,x
    sta banks,x
    clc
    rts
err_not_allocated:
    sec
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

banks:  fill 256
