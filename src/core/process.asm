; Input:
; X/Y: Initial program counter.
init_process:
    stx saved_pc
    sty @(++ saved_pc)
    lda #$ff
    sta saved_stackpointer
    rts

; Input:
; X/Y: Core bank of process.
switch_to_process:
    ; Save BLK1 bank.
    lda $9ff8
    pha
    lda $9ff9
    pha
    jmp @(+ #x2000 (- #x0400 +l))

    ; Switch in process' core bank.
l:  stx $9ff8
    sty $9ff9

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
