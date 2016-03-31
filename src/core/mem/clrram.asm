clrram:
    inc @(++ c)
    lda #0
    tay
l:  sta (d),y
    jsr inc_d
    dec c
    bne -l
    dec @(++ c)
    bne -l
    rts
