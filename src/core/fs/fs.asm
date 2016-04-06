; X: File handle
;
; Returns:
; A: Byte.
fs_read:
    jsr take_over

    lda file_states,x
    tay
    and #FILE_OPENED
    beq +err_not_open

    tya
    and #FILE_READABLE
    beq +err_not_readable

    lda file_vfiles,x
    tax
    ldy #VOP_READ

    jsr call_vfile_op

    clc
    jmp release

err_not_open:
err_not_readable:
    sec
    jmp release

; A: Byte
; X: File handle
fs_write:
    jsr take_over

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
    jsr call_vfile_op

    clc
    jmp release

err_not_open:
err_not_writable:
    pla
    sec
    jmp release
