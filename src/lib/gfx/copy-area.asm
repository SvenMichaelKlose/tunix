.export copy_area

.importzp s, d, scrbase, scr
.importzp xpos, ypos, xpos2, ypos2, width, height
.import calcscr

.proc copy_area
    ; Calculate source screen address.
    lda s
    sta scrbase
    lda s+1
    sta scrbase+1
    jsr calcscr
    lda scr
    sta s
    lda scr+1
    sta s+1

    ; Calculate destination screen address.
    lda xpos
    pha
    lda ypos
    pha
    lda d
    sta scrbase
    lda d+1
    sta scrbase+1
    lda xpos2
    sta xpos
    lda ypos2
    sta ypos
    jsr calcscr
    lda scr
    sta d
    lda scr+1
    sta d+1
    pla
    sta ypos
    pla
    sta xpos

loop:
    ldy height
    dey
l1: lda (s),y
    sta (d),y
    dey
    cpy #255
    bne l1

    lda s
    clc
    adc #12*16
    sta s
    bcc n1
    inc s+1
n1:

    lda d
    clc
    adc #12*16
    sta d
    bcc n2
    inc d+1
n2:

    lda width
    sec
    sbc #8
    sta width
    bcs loop

    rts
.endproc
