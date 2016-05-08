.export frame

.importzp xpos, ypos, width, height
.import vline, hline

.code

.proc frame
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
.endproc
