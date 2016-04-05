clrram:
    ldy d
    ldx c
    lda #0
    sta d
    sta c
    inc @(++ c)
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
