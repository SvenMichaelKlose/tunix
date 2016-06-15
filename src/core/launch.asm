; s: Path name
; A: != 0: Stop parent process until return.
;
; Returns:
; C: Error
.proc launch
    pha
    lda takeovers
    beq ok
    jsr guru_meditation
ok: jsr load_program
    pla
    sta tmp3
    jsr take_over
    clc
    jsr save_process_state  ; Will return from launch on next switch back.
    jsr stop_parent_process
    jsr init_child_process
    lda $9ff4
    jmp switch_to_process
.endproc

.proc load_program
    lda $9ff4
    pha
    jsr load
    bcs error
    pla
    sta tmp2    ; Save parent process' core.
    stx tmp     ; Save new child process' core.
    rts

error:
    pla
    pla
    sta $9ff4
    sec
    rts
.endproc

.proc stop_parent_process
    lda tmp3
    tax
    beq n
    ldx process_slot

    lda $9ff4
    pha
    lda #0
    sta $9ff4

    lda process_states,x
    ora #2      ; Mark as waiting for child process.
    and #$7f
    sta process_states,x

    pla
    sta $9ff4

n:  rts
.endproc

.proc init_child_process
    lda tmp
    sta $9ff4

    ldx tmp2
    stx parent_process

    ldx program_start
    ldy @(++ program_start)
    jsr init_process
    stx current_process
    rts
.endproc
