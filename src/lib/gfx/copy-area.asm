.export copy_area

.importzp s, d, c, scrbase, scr
.importzp xpos, ypos, xpos2, ypos2, width, height, xcpos, xcpos2
.importzp tmp
.import calcscr
.import maskd_left
.import maskd_right
.import masks_left
.import masks_right
.import masksr_left

.code

.proc copy_area
    ; Calculate source screen address.
    lda s
    sta scrbase
    lda s+1
    sta scrbase+1
    jsr calcscr
    lda scr
    sta s
    lda scr+1
    sta s+1

    ; Calculate destination screen address.
    lda xpos
    pha
    lda ypos
    pha
    lda d
    sta scrbase
    lda d+1
    sta scrbase+1
    lda xpos2
    sta xpos
    lda ypos2
    sta ypos
    jsr calcscr
    lda scr
    sta d
    lda scr+1
    sta d+1
    pla
    sta ypos
    pla
    sta xpos

    ; Get number of columns.
    lda width
    lsr
    lsr
    lsr
    sta c

    ;; Calculate shifts.
    lda xpos
    and #7
    sta xcpos
    lda xpos2
    and #7
    sta xcpos2
    sec
    sbc xcpos
    sta xcdiff

    lda #7
    sec
    sbc xcdiff
    sta mod_shift_right+1
    sta mod_shift_right_fast+1
    lda xcdiff
    sta mod_shift_left+1
    sta mod_shift_left_fast+1

    ldx xcpos
    lda masks_left,x
    sta mod_smask_right+1
    lda #$ff
;    lda masksr_left,x
    sta mod_smask_left+1
    ldx xcpos2
    lda maskd_left,x
    sta mod_dmask_right+1
    lda masks_left,x
    sta mod_dmask_left+1

    jsr copy_column_right
    jsr next_destination_column
    jsr copy_column_left_fast

stop:
    lda c
    beq done

loop:
    dec c
    beq rightmost_column

    jsr next_source_column
    jsr copy_column_right_fast
    jsr next_destination_column
    jsr copy_column_left_fast
    jmp loop

rightmost_column:
    jsr next_source_column
    jsr copy_column_right_fast
    jsr next_destination_column
    jsr copy_column_left

done:
    rts

copy_simple:
    ldy height
    dey
l:  lda (s),y
    sta (d),y
    dey
    cpy #255
    bne l
    rts

copy_column_left_fast:
    ldy height
    dey
l5: clc
    lda (s),y
mod_shift_left_fast:
    bcc j5
j5: asl
    asl
    asl
    asl
    asl
    asl
    asl
    asl
    sta (d),y
    dey
    cpy #255
    bne l5
    rts

copy_column_left:
    ldy height
    dey
l2: clc
    lda (s),y
mod_smask_left:
    and #255
mod_shift_left:
    bcc j2
j2: asl
    asl
    asl
    asl
    asl
    asl
    asl
    asl
    sta tmp
    lda (d),y
mod_dmask_left:
    and #255
    ora tmp
    sta (d),y
    dey
    cpy #255
    bne l2
    rts

copy_column_right_fast:
    ldy height
    dey
l3: clc
    lda (s),y
mod_shift_right_fast:
    bcc j3
j3: lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    ora (d),y
    sta (d),y
    dey
    cpy #255
    bne l3
    rts

copy_column_right:
    ldy height
    dey
l4: clc
    lda (s),y
mod_smask_right:
    and #255
mod_shift_right:
    bcc j4
j4: lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    sta tmp
    lda (d),y
mod_dmask_right:
    and #255
    ora tmp
    sta (d),y
    dey
    cpy #255
    bne l4
    rts
.endproc

.proc next_source_column
    lda s
    clc
    adc #12*16
    sta s
    bcc n
    inc s+1
n:  rts
.endproc

.proc next_destination_column
    lda d
    clc
    adc #12*16
    sta d
    bcc n
    inc d+1
n:  rts
.endproc

.data

xcdiff:
    .byte 0
