clrram:
    ldy d
    ldx c
    inx
    lda #0
    sta d
    sta c
l:  sta (d),y
    iny
    beq +m
    dex
    bne -l
    dec @(++ c)
    bne -l
    rts

m:  inc @(++ d)
    jmp -l
