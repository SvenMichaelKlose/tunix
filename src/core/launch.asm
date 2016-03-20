launch:
    jsr overtake
    jsr load
    bcs +error
    txa
    sta $9ff4
    ldx program_start
    ldy @(++ program_start)
    pha
    jsr init_process
    stx process_slot
    lda #0
    sta $9ff4
    stx current_process
    jsr resume
    pla
    jmp switch_to_process
error:
    jmp resume
