.export box

.import masks_left, masks_right, maskd_left, maskd_right
.import calcscr, dec_scr, clip_x, clip_y, add_region_position, sub_region_position, vfill, vcopy, inc_xcpos
.importzp c, scr, xpos, ypos, width, c, xcpos, pattern, masks, maskd

.bss
xposr:  .byte 0

.code

; In: xpos, ypos, width, height

.proc box
    jsr add_region_position
    lda xpos
    pha
    lda width
    pha

    jsr clip_x
    bcc done
    jsr clip_y
    bcc done

    jsr calcscr
    jsr dec_scr

    ; Get width in characters.
    lda xpos
    tay
    clc
    adc width
    sbc #0
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
    lda maskd_left,x
    sta maskd
    lda masks_left,x
    sta masks
    jsr vfill

    ; Draw middle.
    jsr inc_xcpos
    dec c
    beq +right_end
l:  jsr vcopy
    jsr inc_xcpos
    dec c
    bne l

    ; Draw right end.
right_end:
    lda xposr
    and #7
    tax
    lda maskd_right,x
    sta maskd
    lda masks_right,x
    sta masks
    jsr vfill

done:
    pla
    sta width
    pla
    sta xpos
    jmp sub_region_position

single_column:
    ; Draw left end.
    tya
    and #7
    tax
    lda xposr
    and #7
    tay
    lda masks_left,x
    and masks_right,y
    sta masks
    lda maskd_left,x
    ora maskd_right,y
    sta maskd
    jsr vfill
    jmp done
.endproc
