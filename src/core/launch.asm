launch:
    jsr overtake
    jsr load
    bcs +error
    jsr save_process_state
    txa
    ldx #0
    sta $9ff4
    ldx program_start
    ldy @(++ program_start)
    pha
    jsr init_process
    lda #0
    sta $9ff4
    stx current_process
    pla
    jmp switch_to_process
error:
    jmp release
