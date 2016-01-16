box:lda height
    pha
l:  jsr hline
    inc ypos
    dec height
    bne -l
    pla
    sta height
    rts
