launch:
    lda $9ff4
    pha

    ;; Load the program.
    jsr load
    bcs +error
    pla
    stx tmp     ; Save core.

    ;; Save state for switching to it.
    ;; The next task switch back to the current process will return from
    ;; this system call.
    jsr save_process_state

    ;; Stop multitasking.
    jsr take_over

    ;; Initialise process info.
    ; Switch to new process' core.
    lda tmp
    sta $9ff4
    ldx program_start
    ldy @(++ program_start)
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
    sta $9ff4
    sec     ; Signal error.
    rts
