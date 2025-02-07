.export zpw_mov_xy
.export zpw_add_xy
.export zpw_cmp_xy
.export zpw_inc_x
.export zpw_dec_x
.export zpw_addb_xay
.export zpw_is_minus1_y

.code

.proc zpw_mov_xy
    lda 0,x
    sta 0,y
    lda 1,x
    sta 1,y
    rts
.endproc

.proc zpw_add_xy
    lda 0,x
    clc
    adc 0,y
    sta 0,x
    lda 1,x
    adc 1,y
    sta 1,x
    rts
.endproc

.proc zpw_cmp_xy
    lda 1,x
    cmp 1,y
    bcc :+
    bne :+
    lda 0,x
    cmp 0,y
:   rts
.endproc

.proc zpw_inc_x
    inc 0,x
    bne :+
    inc 1,x
:   rts
.endproc

.proc zpw_dec_x
    lda 0,x
    beq :+
    dec 1,x
:   dec 0,x
    rts
.endproc

.proc zpw_addb_xay
    sta 0,y
    lda #0
    sta 1,y
    sta 2,y
    sta 3,y

    lda 0,x
    clc
    adc 0,y
    sta 0,y
    lda 1,x
    adc 1,y
    sta 1,y
    rts
.endproc

.proc zpw_is_minus1_y
    lda 0,y
    and 1,y
    cmp #$ff
    rts
.endproc
