; Input:
; X/Y: Initial program counter.
init_process:
    stx saved_pc
    sty @(++ saved_pc)
    lda #$ff
    sta saved_stackpointer
    rts

; Input:
; A: Core bank of process.
switch_to_process:
    ; Switch in process' core bank.
    sta $9ff8

    ; Restore stack contents.
    ldx saved_stackpointer
l:  lda saved_stack,x
    sta $100,x
    inx
    bne -l

    jsr switch_banks_in

    lda @(++ saved_pc)
    pha
    lda saved_pc
    pha
    lda saved_flags
    pha
    ldx saved_x
    ldy saved_x
    lda saved_a
    rti
