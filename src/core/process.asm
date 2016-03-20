; Input:
; X/Y: Initial program counter.
init_process:
    stx saved_pc
    sty @(++ saved_pc)
    lda #@(high (-- exit_process))
    sta @(+ saved_stack 255)
    lda #@(low (-- exit_process))
    sta @(+ saved_stack 254)
    lda #$fd
    sta saved_sp
    rts

exit_process:
    lda $9ff4
    jsr free_bank
    lda bank1
    beq +n
    jsr free_bank
n:  lda bank2
    beq +n
    jsr free_bank
n:  lda bank3
    beq +n
    jsr free_bank
n:  lda bank5
    beq +n
    jsr free_bank
n:

    lda #0
    sta $9ff4
    ldx current_process
    sta process_states,x

    jmp switch_to_next_process
