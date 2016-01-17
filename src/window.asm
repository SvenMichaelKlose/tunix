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

    lda height
    pha
    lda width
    pha
    lda ypos
    pha
    lda xpos
    pha

    brk
    c_addzb tmp4 xpos width
    0
    lda tmp4
    pha

    brk
    ; Print window title.
    c_addx 2
    c_addy 2
    c_setzw p <txt_clock >txt_clock
    c_apply c_putstring

    ; Draw title grip.
    0
    pla
    sta tmp4

    brk
    c_sbczb width tmp4 xpos
    c_sbczbi width 2
    c_addy 1
    c_apply c_hline
    0
    ldx #2
l:  brk
    c_addy 2
    c_apply c_hline
    0
    dex
    bne -l

    pla
    sta xpos
    pla
    sta ypos
    pla
    sta width
    pla
    sta height

    brk
    c_sbczbi width 2
    c_sbczbi height 11
    c_addy 11
    c_addx 1
    c_setpattern <pat_woven >pat_woven
    c_apply c_box
    0
    
    rts

txt_clock:
    "Clock" 0
