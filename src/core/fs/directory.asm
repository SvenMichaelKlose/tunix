; Read dirent list from BANK_DIRENTS.
;
; tmp8: File
vfile_directory_read:
    ;; Get vfile.
    ldy tmp8
    lda file_vfiles,y
    tax

    ;; Get index into directory list.
    lda file_positions_0,y
    sta tmp
    lda file_positions_1,y
    sta tmp2

    ;; Check if end of file has been reached.
    ; Switch to core data bank.
    lda $9ff4
    pha
    lda #0
    sta $9ff4

    ;; Make pointer into dirent list.
    lda vfile_data_l,x
    clc
    adc tmp
    sta tmp5
    lda vfile_data_h,x
    adc tmp2
    sta tmp6

    ;; Get pointer to dirent list size.
    lda vfile_data_l,x
    sec
    sbc #malloc_chunk_header_size
    sta tmp3
    lda vfile_data_h,x
    sbc #0
    sta tmp4

    ;; Test on end of list.
    lda $9ff8
    pha
    lda #BANK_DIRENTS
    sta $9ff8
 
    ldy #0
    lda (tmp3),y
    cmp tmp
    bne +n
    iny
    lda (tmp3),y
    and #$7f        ; Mask out allocation flag.
    cmp tmp2
    bne +n
    pla
    sta $9ff8
    pla
    sta $9ff4
    lda #EEOF
    sec
    rts
n:

    ;; Read byte.
    ldy #0
    lda (tmp5),y
    sta tmp

    pla
    sta $9ff8
    pla
    sta $9ff4

    ;; Increment pointer.
    ldx tmp8
    inc file_positions_0,x
    bne +n
    inc file_positions_1,x
    bne +n
    inc file_positions_2,x
    bne +n
    inc file_positions_3,x
    bne +n
n:

    lda tmp
    clc
    rts
