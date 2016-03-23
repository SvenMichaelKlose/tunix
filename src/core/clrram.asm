clrram:
    inc @(++ c)
    lda #0
    tay
l:  sta (d),y
    jsr inc_d
    dec c
    bne +n
    dec c
    bne -l
n:  rts
