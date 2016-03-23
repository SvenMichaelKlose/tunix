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

    tay     ; Save core bank.

    ; Find new task slot.
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
    sta process_cores_saved,x

    ; Switch to new process' core.
    sty $9ff4

    ; Save process' slot index.
    stx process_slot

    ; Initialise stack.
    lda #@(high (-- exit_process))
    sta @(+ saved_stack 255)
    lda #@(low (-- exit_process))
    sta @(+ saved_stack 254)
    lda #$fd
    sta saved_sp

    ; Set pointer to first library call to be generated.
    lda #<library_calls
    sta end_of_library_calls
    lda #>library_calls
    sta @(++ end_of_library_calls)
    rts

exit_process:
    lda $9ff4

kill:
    pha
    jsr take_over

    tax
    lda $9ff4
    pha
    stx $9ff4

    ; Exit all linked libraries.
    ldx num_libraries
    beq +no_libraries
    dex
l:  txa
    pha
    lda libraries,x
    jsr kill
    pla
    tax
    dex
    bpl -l
no_libraries:

    jsr free_process_banks

    lda $9ff4
    jsr free_bank

    lda #0
    sta $9ff4
    ldx current_process
    sta process_states,x

    ; Switch to next process if the current one has been killed.
    pla
    sta $9ff4
    pla
    cmp $9ff4
    beq +n
    jmp switch_to_next_process

n:  rts
