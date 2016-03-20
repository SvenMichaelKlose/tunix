launch:
    jsr load
    bcs +error
    pha
    ldx program_start
    ldy @(++ program_start)
    jsr init_process
    pla
    jmp switch_to_process
error:
    rts
