alloc_bank:
    txa
    pha
    tya
    pha

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

return:
    pla
    tay
    pla
    tax
    rts

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
    clc
    jmp -return

free_bank:
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
