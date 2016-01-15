calc_scr:
    lda xpos
    lsr
    lsr
    lsr
    tax
    lda ypos
    clc
    adc column_addrs_lo,x
    sta scr
    lda column_addrs_hi,x
    clc
    adc #0
    sta @(++ scr)
    rts

column_addrs_lo: @(maptimes [low (+ charset (* 16 screen_rows _))] screen_columns)
column_addrs_hi: @(maptimes [high (+ charset (* 16 screen_rows _))] screen_columns)

fill_column:
    jsr calc_scr

    ldy height
    dey
l:  lda (scr),y
    and mask
    ora line
    sta (scr),y
    dey
    cpy #255
    bne -l

    rts
