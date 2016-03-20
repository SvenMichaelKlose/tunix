launch:
    jsr load
    bcs +error
    txa
    sta $9ff4
    ldx program_start
    ldy @(++ program_start)
    jsr init_process
    lda $9ff4
    jmp switch_to_process
error:
    rts
