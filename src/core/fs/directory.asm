; Read dirent list from BANK_DIRENTS.
vfile_directory_read:
    ;; Get index into directory list.
    lda file_positions_0,y
    sta tmp
    lda file_positions_1,y
    sta tmp2
    sty tmp3
    lda file_vfiles,y
    tax

    ;; Check if end of file has been reached.
    lda $9ff4
    pha
    lda #0
    sta $9ff4

    lda vfile_sizes_0,x
    cmp tmp
    bne +n

    lda vfile_sizes_1,x
    cmp tmp2
    bne +n
    pla
    sta $9ff4
    lda #EEOF
    sec
    rts
n:

    ;; Make pointer into dirent list.
    lda vfile_data_l,x
    clc
    adc tmp
    sta tmp
    lda vfile_data_h,x
    clc
    adc tmp2
    sta tmp2

    ;; Read byte.
    ldx $9ff8
    lda #BANK_DIRENTS
    sta $9ff8
    ldy #0
    lda (tmp),y
    sta tmp
    stx $9ff8

    pla
    sta $9ff4

    ;; Increment pointer.
    ldx tmp3
    inc file_positions_0,x
    bne +n
    inc file_positions_1,x
n:

    lda tmp
    clc
    rts
