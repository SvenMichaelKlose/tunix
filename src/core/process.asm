; Input:
; X/Y: Initial program counter.
init_process:
    stx saved_pc
    sty @(++ saved_pc)
    lda #$ff
    sta saved_sp
    rts

exit_process:
    rts
