return_enosys:
    lda #ENOSYS
    sec
    rts

release_with_error:
    sec
    jmp release
