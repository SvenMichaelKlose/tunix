window:
    brk
    c_setpattern <pat_empty >pat_empty
    c_apply c_box
    c_setpattern <pat_solid >pat_solid
    c_apply c_frame
    0

    ; Draw bottom of title.
    lda ypos
    pha
    clc
    adc #10
    sta ypos
    jsr hline
    pla
    sta ypos

    ; Get rightmost X position.
    lda xpos
    clc
    adc width
    sta xright

    ; Print window title.
    inc xpos
    inc xpos
    inc ypos
    inc ypos
    lda #<txt_clock
    sta p
    lda #>txt_clock
    sta @(++ p)
    jsr putstring

    ; Draw title grip.
    lda xright
    sec
    sbc xpos
    bcc +done
    sbc #2
    bcc +done
    sta width
    inc ypos
    jsr hline
    inc ypos
    inc ypos
    jsr hline
    inc ypos
    inc ypos
    jsr hline

done:
    rts

txt_clock:
    "Clock" 0

xright: 0
