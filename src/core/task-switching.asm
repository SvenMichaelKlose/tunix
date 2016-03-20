switch:
    jsr overtake

    ;;; Save process status.
    ; Save registers.
    sta saved_a
    stx saved_x
    sty saved_y
    pla
    sta saved_flags
    pla
    sta saved_pc
    pla
    sta @(++ saved_pc)
    tsx
    sta saved_sp

    ; Save stack.
l:  lda $100,x
    sta saved_stack,x
    inx
    bne -l

    ; Save set of banks.
    lda $9ff8
    sta saved_blk1
    lda $9ffa
    sta saved_blk2
    lda $9ffc
    sta saved_blk3
    lda $9ffd
    sta saved_blk5

    ;;; Get next process.
    ; Switch to master core.
    lda #0
    sta $9ff4

switch_to_next_process:
    ldx current_process
m:  inx
    cpx #max_num_processes
    bne +l
    ldx #0
l:  lda process_states,x
    bpl -m      ; Not running.

    ;;; Switch to found process.
    lda process_cores,x
    stx current_process

; Input:
; A: Core bank of process.
switch_to_process:
    ; Switch in process' core bank.
    sta $9ff4

    ; Restore stack contents.
    ldx saved_sp
    txs
l:  lda saved_stack,x
    sta $100,x
    inx
    bne -l

    lda saved_blk1
    sta $9ff8
    lda saved_blk2
    sta $9ffa
    lda saved_blk3
    sta $9ffc
    lda saved_blk5
    sta $9ffe

    lda @(++ saved_pc)
    pha
    lda saved_pc
    pha
    lda saved_flags
    pha
    lda saved_a
    ldx saved_x
    ldy saved_y

    jsr release
    rti

save_process_state:
    ; Save register contents.
    sta saved_a
    stx saved_x
    sty saved_y
    php
    pla
    sta saved_flags

    ; Set return address to RTS that'll return from system call.
    lda #<+return
    sta saved_pc
    lda #>+return
    sta @(++ saved_pc)

    ; Save stack.
    tsx
    inx         ; Undo return address of this procedure.
    inx
    stx saved_sp
l:  lda $100,x
    sta saved_stack,x
    inx
    bne -l

    ; Restore registers destroyed by thos procedure except the flags.
    lda saved_a
    ldx saved_x

return:
    rts

overtake:
    pha
    lda #$7F
    sta $911e
    pla
    inc takeovers
    rts

release:
    dec takeovers
    beq restart_task_switching
    rts

restart_task_switching:
    sei
    pha

    ; Disable NMI.
    lda #$7f
    sta $911e

    ; Enable Timer #1 on VIA 1 for NMI.
    lda #<switch
    sta $0318
    lda #>switch
    sta $0319   

    ; Load timer.
    lda #$00
    sta $9114
    lda #$80
    sta $9115

    ; Re–enable NMI.
    lda #$40        ; free–running
    sta $911b
    lda #$c0
    sta $911e

    pla
    cli
    rts
