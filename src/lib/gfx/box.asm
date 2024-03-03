.export box

.import masks_left, masks_right, maskd_left, maskd_right
.import calcscr, dec_scr, clip_x, clip_y
.import add_region_position, sub_region_position, vfill, vcopy, inc_xcpos
.importzp c, scr, xpos, ypos, width, height, c, xcpos, pattern, masks, maskd

    .bss

xposr:  .res 1
tmp:    .res 1

    .code

; In: xpos, ypos, width, height

.proc box
    lda width
    beq exit
    lda height
    bne l3
exit:
    rts

l3: dec width

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
    lda scr     ; One off in VFILL.
    sec
    sbc #1
    sta scr
    bcs l2
    dec scr+1

    ; Get width in characters.
l2: lda xpos
    tay
    lsr
    lsr
    lsr
    sta tmp
    tya
    clc
    adc width
    sta xposr
    lsr
    lsr
    lsr
    sec
    sbc tmp
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
