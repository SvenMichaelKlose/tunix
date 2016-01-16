window:
    lda #<pat_empty
    sta pattern
    lda #>pat_empty
    sta @(++ pattern)
    jsr box
    lda #<pat_solid
    sta pattern
    lda #>pat_solid
    sta @(++ pattern)
    jsr frame

    lda ypos
    pha
    clc
    adc #10
    sta ypos
    jsr hline
    pla
    sta ypos

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
