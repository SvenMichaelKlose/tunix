frame:
    jsr vline
    jsr hline
    lda xpos
    pha
    clc
    adc width
    sta xpos
    jsr vline
    pla
    sta xpos
    lda ypos
    pha
    clc
    adc height
    sta ypos
    jsr hline
    pla
    sta ypos
    rts
