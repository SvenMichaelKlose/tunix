devcon_keyboard_vops:
    <devcon_read_keyboard >devcon_read_keyboard ; read
    <devcon_error >devcon_error ; write
    <devcon_error >devcon_error ; lookup

devcon_screen_vops:
    <devcon_error >devcon_error ; read
    <devcon_write_screen >devcon_write_screen ; write
    <devcon_error >devcon_error ; lookup

devcon_init:
    lda #$93            ; Clear screen.
    jsr chrout
    lda #%11110010      ; Up/locase chars.
    sta $9005
    lda #$00            ; Blink cursor.
    sta $cc             ; (BLNSW)

    lda #FILE_OPENED
    sta vfile_states
    sta @(++ vfile_states)
    sta @(+ 2 vfile_states)
    ldy #0
    sty vfile_handles
    iny
    sty @(++ vfile_handles)
    sty @(+ 2 vfile_handles)
    lda #CBMDEV_KEYBOARD
    sta devcon_logical_file_numbers
    lda #<devcon_keyboard_vops
    sta vfile_ops_l
    lda #>devcon_keyboard_vops
    sta vfile_ops_h
    lda #CBMDEV_SCREEN
    sta @(++ devcon_logical_file_numbers)
    sta @(+ 2 devcon_logical_file_numbers)
    lda #<devcon_screen_vops
    sta @(++ vfile_ops_l)
    sta @(+ 2 vfile_ops_l)
    lda #>devcon_screen_vops
    sta @(++ vfile_ops_h)
    sta @(+ 2 vfile_ops_h)

    ; Initialse stdin.
    lda #CBMDEV_KEYBOARD
    ldx #CBMDEV_KEYBOARD
    ldy #0              ; (read)
    jsr setlfs
    jsr open

    ; Initialse stdout and stderr.
    lda #CBMDEV_SCREEN
    ldx #CBMDEV_SCREEN  ; Screen
    ldy #1              ; (write)
    jsr setlfs
    jmp open

devcon_error:
    sec
    rts

devcon_read_keyboard:
    jsr take_over
    jsr wait_key
    jmp release

; X: vfile index
devcon_read:
    jsr take_over
    lda vfile_handles,x
    tax
    lda devcon_logical_file_numbers,x
    tax
    jsr chkin
    jsr chrin
    jmp release

; X: vfile index
; A: character
devcon_write_screen:
    tay
    beq +r

devcon_write:
    jsr take_over

    pha
    lda vfile_handles,x
    tax
    lda devcon_logical_file_numbers,x
    tax
    jsr chkout
    pla
    jsr chrout

    jmp release

r:  rts
