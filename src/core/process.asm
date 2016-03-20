; Input:
;   A: Process core.
;   X/Y: Initial program counter.
;
; Returns:
;   X: Process slot.
init_process:
    sta $9ff4
    stx saved_pc
    sty @(++ saved_pc)

    ; Find new task slot.
    tay     ; Save core bank.
    lda #0
    sta $9ff4
    ldx current_process
l:  inx
    cpx #max_num_processes
    beq -l
    lda process_states,x
    bne -l

    ; Mark slot as being taken (1) and process as running (128).
    lda #129
    sta process_states,x

    ; Save core bank.
    tya
    sta process_cores,x

    ; Switch to new process' core.
    sty $9ff4

    ; Save process' slot index.

    ; Initialise stack.
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
    inc takeovers
    ldx current_process
    sta process_states,x

    jmp switch_to_next_process
