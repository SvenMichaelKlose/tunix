devcon_screen_vops:
    <devcon_read >devcon_read
    <devcon_write_screen >devcon_write

devcon_init:
    lda #$93            ; Clear screen.
    jsr chrout
    lda #%11110010      ; Up/locase chars.
    sta $9005

    lda #FILE_OPEN
    sta vfile_states
    lda #0
    sta vfile_handles
    lda #CBMDEV_SCREEN
    sta devcon_logical_file_numbers
    lda #<devcon_screen_vops
    sta vfile_ops_l
    lda #>devcon_screen_vops
    sta vfile_ops_h

    ; Initialse stdout and stderr.
    lda #CBMDEV_SCREEN
    ldx #CBMDEV_SCREEN  ; Screen
    ldy #1              ; (write)
    jsr setlfs
    jmp open

; X: vfile index
devcon_read:
    pha
    lda vfile_handles,x
    tax
    lda devcon_logical_file_numbers,x
    tax
    jsr chkin
    pla
    jmp chrin

; X: vfile index
; A: character
devcon_write_screen:
    cmp #@(char-code #\A)
    bcc +n
    cmp #@(++ (char-code #\Z))
    bcs +n
    sbc #@(- (char-code #\A) (char-code #\a) 1)
    jmp +l

n:  cmp #@(char-code #\a)
    bcc +l
    cmp #@(++ (char-code #\z))
    bcs +l
    sbc #@(- (char-code #\a) (char-code #\A) 1)

devcon_write:
l:  jsr take_over

    pha
    lda vfile_handles,x
    tax
    lda devcon_logical_file_numbers,x
    tax
    jsr chkout
    pla
    jsr chrout

    jmp release
