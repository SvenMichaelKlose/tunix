set_error:
    sec
    rts

release_with_error:
    jsr set_error
    jmp release

get_error:
    lda last_error
    rts
