set_error:
    sta last_error
    sec
    rts

return_error:
    jsr set_error
    jmp release

get_error:
    lda last_error
    rts
