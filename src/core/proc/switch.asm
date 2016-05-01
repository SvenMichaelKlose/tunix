; Calls over "take_over"s that haven't been "resume"d.

force_switch:
    ; Increment return address for RTI.
    stx saved_x
    tsx
    inc $0102,x
    ldx saved_x

switch:
    pha

    ;; Restart NMI.
    lda #$80
    sta $9115

    ;; Return if multitasking has been turned off.
    lda takeovers
    beq +l
    sta needs_switch
    pla
    rti

l:  pla
    jsr take_over

    ;; Save process status.
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
    stx saved_sp

    ; Save stack.
l:  lda $100,x
    sta saved_stack,x
    inx
    bne -l

    ; Save zero page.
    ldx #$8f
l:  lda 0,x
    sta saved_zeropage,x
    dex
    bne -l
    lda 0
    sta saved_zeropage

    ; Save set of banks.
    lda $9ff6
    sta saved_bank_io
    lda $9ff8
    sta saved_bank1
    lda $9ffa
    sta saved_bank2
    lda $9ffc
    sta saved_bank3
    lda $9ffe
    sta saved_bank5
    ldy $9ff4
    ldx process_slot
    lda #0
    sta $9ff4
    tya
    sta process_cores_saved,x


switch_to_next_process:
    ; Switch to master core.
    lda #0
    sta $9ff4

    ; Get next process.
    ldx current_process
    ldy #max_num_processes
m:  dey
    bmi +g
    inx
    cpx #max_num_processes
    bne +l
    ldx #0
l:  lda process_states,x
    bpl -m      ; Not running.

    ;;; Switch to found process.
    lda process_cores_saved,x

; Switch to particular process.
;
; Input:
; A: Core bank of process.
switch_to_process:
    ; Switch in process' core bank.
    sta $9ff4

    ; Keep next call of "release" from forcing a switch.
    lda #0
    sta needs_switch

    lda process_slot
    sta current_process

    ; Restore stack contents.
    ldx saved_sp
    txs
l:  lda saved_stack,x
    sta $100,x
    inx
    bne -l

    ; Restore zero page.
    ldx #$8f
l:  lda saved_zeropage,x
    sta 0,x
    dex
    bne -l
    lda saved_zeropage
    sta 0

    lda saved_bank_io
    sta $9ff6
    lda saved_bank1
    sta $9ff8
    lda saved_bank2
    sta $9ffa
    lda saved_bank3
    sta $9ffc
    lda saved_bank5
    sta $9ffe

return_from_switch:
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

g:  jsr guru_meditation

save_process_state:
    ; Save register contents.
    sta saved_a
    stx saved_x
    sty saved_y
    php
    pla
    sta saved_flags

    ; Save actually used core bank.
    ldy $9ff4
    ldx process_slot
    lda #0
    sta $9ff4
    tya
    sta process_cores_saved,x
    sty $9ff4

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

    ; Save zero page.
    ldx #$9f
l:  lda 0,x
    sta saved_zeropage,x
    dex
    bpl -l

    ; Restore registers destroyed by this procedure except the flags.
    lda saved_a
    ldx saved_x

return:
    rts

take_over:
    php
    inc takeovers
    beq +g
    plp
    rts

g:  jsr guru_meditation

release:
    php
    dec takeovers
    beq +n
    pha
    lda takeovers
    cmp #$ff
    beq +g
    pla
    plp
    rts

g:  jsr guru_meditation

n:  pha
    lda needs_switch
    bne +n
    pla
    plp
    rts

n:  pla
    jmp force_switch

stop_task_switching:
    php
    pha

    ;; Disable NMI.
    lda #$7f
    sta $911e

    ;; Restore old NMI vector.
    lda old_nmi
    sta $318
    lda @(++ old_nmi)
    sta $319

    pla
    plp
    rts

start_task_switching:
    php
    pha

    ;; Save old NMI vector.
    lda $318
    sta old_nmi
    lda $319
    sta @(++ old_nmi)

    ;; Set NMI vector to task switcher.
    lda #<switch
    sta $318
    lda #>switch
    sta $319

    ;; Load timer.
    lda #$00
    sta $9114
    lda #$80
    sta $9115

    ;; Re–enable NMI.
    lda #$40        ; free–running
    sta $911b
    lda #$c0
    sta $911e

    pla
    plp
    rts
