set_error:
    sta last_error
    sec
    jmp release
