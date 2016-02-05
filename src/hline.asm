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
    lda xpos
    pha
    lda width
    pha

    ; Clip
    lda ypos
    cmp ryt
    bcc +done
    cmp ryb
    beq +n
    bcs +done
n:  jsr clip_x
    bcc +done

    jsr calcscr

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
    and maskd_left,x
    sta tmp
    lda tmp2
    and masks_left,x
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
    and maskd_right,x
    sta tmp
    lda tmp2
    and masks_right,x
    ora tmp
    sta (scr),y
done:
    pla
    sta width
    pla
    sta xpos
    rts

single_column:
    ; Draw left end.
    tya
    and #7
    tax
    lda xposr
    and #7
    tay
    lda masks_left,x
    and masks_right,x
    sta tmp3
    lda maskd_left,x
    ora maskd_right,x
    ldy #0
    and (scr),y
    sta tmp
    lda tmp2
    and tmp3
    ora tmp
    sta (scr),y
    jmp -done
