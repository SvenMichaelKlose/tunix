    .export box

    .import height, ypos, hline

    .segment "CODE"

.proc box
    lda height
    pha
    lda ypos
    pha
l:  jsr hline
    inc ypos
    dec height
    bne l
    pla
    sta ypos
    pla
    sta height
    rts
.endproc
