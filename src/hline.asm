inc_xcpos:
    lda scr
    clc
    adc #@(* 16 screen_rows)
    sta scr
    bcc +n
    inc @(++ scr)
n:  rts

; In: xpos, ypos, width
xposr:  0

hline:
    jsr calc_scr

    ; Get pattern.
    lda ypos
    and #7
    tay
    lda (pattern),y
    sta tmp2

    ; Get width in characters.
    lda xpos
    tay
    clc
    adc width
    sta xposr
    lsr
    lsr
    lsr
    sec
    sbc xcpos
    beq single_column
    sta c

    ; Draw left end.
    tya
    and #7
    tax
    ldy #0
    lda (scr),y
    and hmaskd_left,x
    sta tmp
    lda tmp2
    and hmasks_left,x
    ora tmp
    sta (scr),y

    ; Draw middle.
    jsr inc_xcpos
    dec c
    beq +right_end
l:  lda tmp2
    sta (scr),y
    jsr inc_xcpos
    dec c
    bne -l

    ; Draw right end.
right_end:
    lda xposr
    and #7
    tax
    lda (scr),y
    and hmaskd_right,x
    sta tmp
    lda tmp2
    and hmasks_right,x
    ora tmp
    sta (scr),y
    rts

single_column:
    ; Draw left end.
    tya
    and #7
    tax
    lda xposr
    and #7
    tay
    lda hmasks_left,x
    and hmasks_right,x
    sta tmp3
    lda hmaskd_left,x
    ora hmaskd_right,x
    ldy #0
    and (scr),y
    sta tmp
    lda tmp2
    and tmp3
    ora tmp
    sta (scr),y
    rts

hmasks_left:
    %11111111
hmaskd_right:
    %01111111
    %00111111
    %00011111
    %00001111
    %00000111
    %00000011
    %00000001
hmaskd_left:
    %00000000
hmasks_right:
    %10000000
    %11000000
    %11100000
    %11110000
    %11111000
    %11111100
    %11111110
    %11111111
