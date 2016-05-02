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

    c_pushz xpos context_size
    c_addzb tmp4 xpos width
    0
    lda tmp4
    pha

    ; Print window title.
    brk
    c_setzb font $30
    c_addx 2
    c_addy 2
    c_setzw p <txt_clock >txt_clock
    c_apply c_putstring
    0

    ; Draw title grip.
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

    brk
    c_popz xpos context_size
    c_sbczbi width 2
    c_sbczbi height 11
    c_addy 11
    c_addx 1
    c_setpattern <pat_empty >pat_empty
    c_apply c_box
    0
    
    rts

txt_clock:
    "untitled" 0
