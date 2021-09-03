.export putchar_fixed
.importzp xpos, scr, font
.import calcscr

    .zeropage

tmp:    .res 1
tmp2:   .res 1
tmp3:   .res 1

    .code

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
    bne l2

l1:
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    ora tmp3
    sta (scr),y

    jmp done

l2:
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y
    dey
    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    ora tmp3
    sta (scr),y

done:
    rts
.endproc
