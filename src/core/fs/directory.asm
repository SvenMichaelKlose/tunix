; Read dirent list from BANK_DIRENTS.
vfile_directory_read:
    ;; Save file.
    sty tmp3

    ;; Get index into directory list.
    lda file_positions_0,y
    sta tmp
    lda file_positions_1,y
    sta tmp2

    ;; Get vfile.
    lda file_vfiles,y
    tax

    ;; Check if end of file has been reached.
    ; Switch to core data bank.
    lda $9ff4
    pha
    lda #0
    sta $9ff4

    ; Get pointer into dirent list.
    lda tmp
    clc
    adc vfile_data_l,x
    sta tmp
    lda tmp2
    adc vfile_data_h,x
    sta tmp2

    ; Get dirent chunk size.
    lda vfile_data_l,x
    sec
    sbc #malloc_chunk_header_size
    sta tmp3
    lda vfile_data_h,x
    sbc #0
    sta tmp4

    ; Correct it.
    lda tmp3
    sec
    sbc #malloc_chunk_header_size
    sta tmp3
    bcs +n
    dec tmp4

    ; Compare.
    lda tmp
    cmp tmp3
    bne +n
    lda tmp2
    cmp tmp4
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
