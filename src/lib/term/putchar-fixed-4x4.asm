.export putchar_fixed
.importzp xpos, scr, font, pencil_mode
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

    lda pencil_mode
    cmp #2
    bne n
    jmp reverse
n:

    lda xpos
    and #4
    bne l1

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

l1:
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

l2: jmp l3

reverse:
    lda xpos
    and #4
    bne l2

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%00001111
    sta tmp3
    lda (tmp),y
    and #%11110000
    eor #%11110000
    ora tmp3
    sta (scr),y

    jmp done

l3: lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y
    dey

    lda (scr),y
    and #%11110000
    sta tmp3
    lda (tmp),y
    and #%00001111
    eor #%00001111
    ora tmp3
    sta (scr),y

    jmp done

.endproc
