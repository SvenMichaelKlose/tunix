init_per_process_data:
    lda #<per_process_data_start
    sta d
    lda #>per_process_data_start
    sta @(++ d)
    lda #<per_process_data_size
    sta c
    lda #>per_process_data_size
    sta @(++ c)
    jsr clrram

    ; Make jump to link().
    lda #$4c
    sta $0400
    lda #<link
    sta $0401
    lda #>link
    sta $0402

    lda #BANK_CORE_CODE
    sta saved_bank5

    ; Initialise standard streeams.
    lda #@(+ FILE_OPENED FILE_READABLE)
    sta file_states
    lda #@(+ FILE_OPENED FILE_WRITABLE)
    sta @(++ file_states)
    lda #@(+ FILE_OPENED FILE_WRITABLE)
    sta @(+ 2 file_states)
    ldy #0
    sty file_vfiles
    iny
    sty @(++ file_vfiles)
    sty @(+ 2 file_vfiles)
    sty @(+ 3 file_vfiles)

    ; Initialise path working directory.
    lda #@(+ FILE_OPENED FILE_DIRECTORY)
    sta @(+ 3 file_states)
    lda $9ff4
    pha
    lda #0
    sta $9ff4
    ldy vfile_root
    pla
    sta $9ff4
    sty @(+ 3 file_vfiles)
    lda #3
    sta pwd

    rts

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
    lda #0
    sta $9ff4

    ; Find new task slot.
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
    ;; Save process' core.
    sta tmp

    ;; Save X register for recursions.
    txa
    pha

    ;; Stop multitasking.
    jsr take_over

    ;; Save current process' core and switch to one's to kill.
    lda $9ff4
    pha
    lda tmp
    sta $9ff4
    pha

    ;; Resume waiting parent process.
    ldy parent_process
    sty $9ff4
    ldx process_slot
    lda #0
    sta $9ff4
    lda process_states,x
    and #%00000010
    beq +n
    tya
    jsr resume
n:  pla
    sta $9ff4
    pha

    ;; Kill libraroes of process.
    ldx num_libraries
    beq +no_libraries
    dex
l:  lda libraries,x
    jsr kill
    dex
    bpl -l
no_libraries:

    ;; Free banks of process.
    jsr free_process_banks

    ;; Save process' slot.
    lda process_slot
    pha

    ;; Free core of process.
    lda $9ff4
    ldx #0
    stx $9ff4
    jsr free_bank

    ;; Free process slot.
    pla
    tax
    lda #0
    sta process_states,x

    ; Switch to next process if the current one has been killed.
    pla
    sta $9ff4
    pla
    cmp $9ff4
    bne +n
    pla
    jmp switch_to_next_process

    ;; Switch back to callee's core and return.
n:  sta $9ff4
    pla
    tax
    jmp release

halt:
    tax
    lda $9ff4
    pha
    stx $9ff4
    ldx process_slot
    lda #0
    sta $9ff4
    lda process_states,x
    and #%01111111
    sta process_states,x
    pla
    sta $9ff4
    rts

resume:
    tax
    lda $9ff4
    pha
    stx $9ff4
    ldx process_slot
    lda #0
    sta $9ff4
    lda process_states,x
    and #%11111101
    ora #%10000000
    sta process_states,x
    pla
    sta $9ff4
    rts
