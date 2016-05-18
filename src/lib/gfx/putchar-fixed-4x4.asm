.export putchar_fixed
.importzp xpos, scr, font, tmp, tmp2
.import calcscr

.proc putchar_fixed
    ; Calculate character address.
    ldy #0
    sty tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    clc
    adc font
    sta tmp
    lda tmp2
    adc font+1
    sta tmp2

    jsr calcscr

    ldy #7
    lda xpos
    and #4
    beq l2

l1: lda (tmp),y
    lsr
    lsr
    lsr
    lsr
    ora (scr),y
    sta (scr),y
    dey
    bpl l1
    bmi done

l2: lda (tmp),y
    ora (scr),y
    sta (scr),y
    dey
    bpl l2

done:
    lda xpos
    clc
    adc #4
    sta xpos

    rts
.endproc
