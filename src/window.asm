window:
    brk
    c_setpattern <pat_empty >pat_empty
    c_apply c_box
    c_setpattern <pat_solid >pat_solid
    c_apply c_frame

    ; Draw bottom of title.
    c_addy 10
    c_apply c_hline
    c_addy @(- 256 10)
    0

    ; Get rightmost X position.
    lda xpos
    clc
    adc width
    sta xright

    ; Print window title.
    brk
    c_addx 2
    c_addy 2
    c_setzw p <txt_clock >txt_clock
    c_apply c_putstring
    0

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
    ldx #2
l:  brk
    c_addy 2
    c_apply c_hline
    0
    dex
    bne -l

done:
    rts

txt_clock:
    "Clock" 0

xright: 0
