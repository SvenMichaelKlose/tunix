; X: File handle
;
; Returns:
; A: Byte.
fs_read:
    lda takeovers
    pha
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
    bcs +r

    jsr release

    pla
    cmp takeovers
    bne +g
    rts
g:  jsr guru_meditation

err_not_open:
err_not_readable:
    sec
r:  jmp release

; A: Byte
; X: File handle
fs_write:
    sta tmp
    lda takeovers
    pha
    lda tmp
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
    bcs +e

    clc
    jsr release
    pla
    cmp takeovers
    bne +g
    rts
g:  jsr guru_meditation

err_not_open:
err_not_writable:
    sec
    pla
e:  pla
    jmp release
