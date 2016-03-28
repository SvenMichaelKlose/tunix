; X: File handle
;
; Returns:
; A: Byte.
fs_read:
    pha
    lda file_states,x
    tay
    and #FILE_OPENED
    beq +err_not_open

    tya
    and #FILE_READABLE
    beq +err_not_writable

    lda file_vfiles,x
    tax
    ldy #VOP_READ
    pla
    jmp call_vfile_op

err_not_open:
err_not_writable:
    pla
    sec
    rts

; A: Byte
; X: File handle
fs_write:
    pha
    lda file_states,x
    tay
    and #FILE_OPENED
    beq +err_not_open

    tya
    and #FILE_WRITABLE
    beq +err_not_writable

    lda file_vfiles,x
    tax
    ldy #VOP_WRITE
    pla
    jmp call_vfile_op

err_not_open:
err_not_writable:
    pla
    sec
    rts
