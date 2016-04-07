set_error:
    sta last_error
    sec
    jmp release

get_error:
    lda last_error
    rts
