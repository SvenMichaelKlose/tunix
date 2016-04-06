devcon_print_string:
    ldy #0
l:  lda (s),y
    beq +done
    jsr devcon_print_ctrl
    jsr inc_s
    jmp -l

done:
    rts

devcon_print_ctrl:
    sta tmp2
    txa
    pha
    tya
    pha

    lda tmp2
    beq +done
    cmp #10
    bne +l
    lda #0
    sta xpos
    lda ypos
    clc
    adc #8
    sta ypos
    jmp +done
    jmp +l

devcon_print:
    sta tmp2
    txa
    pha
    tya
    pha

l:  lda tmp2
    ldy #0
    sty tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    clc
    adc #<charset_4x8
    sta tmp
    lda tmp2
    adc #>charset_4x8
    sta tmp2
    
    jsr calcscr

    lda xpos
    and #4
    bne +n

    ldy #7
l:  lda (tmp),y
    asl
    asl
    asl
    asl
    ora (scr),y
    sta (scr),y
    dey
    bpl -l
    bmi +m

n:  ldy #7
l:  lda (tmp),y
    ora (scr),y
    sta (scr),y
    dey
    bpl -l

m:  lda xpos
    clc
    adc #4
    sta xpos
    cmp #screen_width
    bne +n
    lda #0
    sta xpos
    lda ypos
    clc
    adc #8
    sta ypos
    cmp #screen_height
    bne +n
n:  

done:
    pla
    tay
    pla
    tax
    rts
