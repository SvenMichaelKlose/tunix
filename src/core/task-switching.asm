switch:
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

    ;;; Get next process.
    ; Switch to master core.
    lda #0
    sta $9ff4

    ldx current_process
m:  inx
    cpx #max_num_processes
    bne +l
    ldx #0
l:  lda process_states,x
    bpl -m      ; Not running.

    ;;; Switch to found process.
    lda process_cores,x

; Input:
; A: Core bank of process.
switch_to_process:
    ; Switch in process' core bank.
    sta $9ff4

    ; Restore stack contents.
    ldx saved_sp
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
    ldy saved_y
    lda saved_a
    rti

init_task_switching:
    ; Disable interrupts and NMI.
    sei
    lda #$7F
    sta $911e

    ; Enable Timer #1 on VIA 1 for NMI.
    lda #$40
    sta $911b

    lda #<switch
    sta $0318
    lda #>switch
    sta $0319   

    ; Load timer.
    lda #$00
    sta $9116
    lda #$80
    sta $9117

    ; Re–enable NMI.
    lda #$c0
    sta $911e
    cli

    rts
