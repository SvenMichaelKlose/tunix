devcon_init:
    lda #FILE_OPEN
    sta vfile_states
    sta @(++ vfile_states)

    ; Initialse stdout and stderr.
    lda #0
    ldx #3      ; Screen
    ldy #0
    jsr setlfs
    jmp open

devcon_read:
    pha
    lda devcon_logical_file_numbers,x
    tax
    jsr chkin
    pla
    jmp chrin

devcon_write:
    pha
    lda devcon_logical_file_numbers,x
    tax
    jsr chkout
    pla
    jmp chrout

devcon_close:
    lda devcon_logical_file_numbers,x
    jmp close
