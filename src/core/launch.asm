g:  jsr guru_meditation

; s: Path name
; A: != 0: Stop parent process until return.
launch:
    pha
    lda takeovers
    bne -g
    lda $9ff4
    pha

    ;; Load the program.
    jsr load
    bcs +error
    pla
    sta tmp2    ; Save parent process' core.
    stx tmp     ; Save new child process' core.

    pla
    sta tmp3

    ;; Save state for switching to it.
    ;; The next task switch back to the current process will return from
    ;; this system call.
    jsr save_process_state

    ;; Stop multitasking.
    jsr take_over

    ;; Stop parent process.
    lda tmp3
    tax
    beq +n
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
n:

    ;; Initialise process info.
    ; Switch to new process' core.
    lda tmp
    sta $9ff4

    ; Save parent process.
    ldx tmp2
    stx parent_process

    ; Get program start.
    ldx program_start
    ldy @(++ program_start)

    ; Init per–process data.
    jsr init_process

    ; Save process info slot index.
    lda $9ff4
    ldy #0
    sty $9ff4
    stx current_process

    ;; Run the new process.
    jmp switch_to_process

error:
    ;; Enable multitasking again and return.
    pla
    pla
    sta $9ff4
    sec     ; Signal error.
    rts
