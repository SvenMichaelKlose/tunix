print_hex:
    pha
    lsr
    lsr
    lsr
    lsr
    jsr print_nibble
    pla
    and #$0f

print_nibble:
    cmp #10
    bcc +n
    clc
    adc #@(- (char-code #\A) 10)
    jmp devcon_print
n:  adc #@(char-code #\0)
    jmp devcon_print

guru_line:
    ldx #@(- (* 2 screen_columns) 2)
l:  jsr devcon_print
    dex
    bne -l
    rts

guru_middle:
    lda #$ba
    jsr devcon_print
    lda #$20
    jsr guru_line
    lda #$ba
    jmp devcon_print

guru_meditation:
    sei
    lda #$7f
    sta $911e
    sta $912e

    jsr init_bitmap_mode
    lda #%00001000
    sta $900f
    jsr devcon_clear_screen
    lda #red
    jsr fill_colors

    lda #$c9
    jsr devcon_print
    lda #$cd
    jsr guru_line
    lda #$bb
    jsr devcon_print

    jsr guru_middle
    jsr guru_middle

    lda #$c8
    jsr devcon_print
    lda #$cd
    jsr guru_line
    lda #$bc
    jsr devcon_print

    ; Print guru message.
    lda #8
    sta xpos
    lda #8
    sta ypos
    lda #<txt_guru
    sta s
    lda #>txt_guru
    sta @(++ s)
    jsr devcon_print_string

    ; Get to next line.
    lda #12
    sta xpos
    lda #16
    sta ypos

    lda #<txt_guru_error
    sta s
    lda #>txt_guru_error
    sta @(++ s)
    jsr devcon_print_string

    ; Print error code.
    lda last_error
    jsr print_hex
    lda #$20
    jsr devcon_print

    ; Print current banks.
    ldx #0
l:  lda $9ff4,x
    jsr print_hex
    lda #@(char-code #\:)
    jsr devcon_print
    inx
    inx
    cpx #12
    bne -l

    ; Print callee's address.
    pla
    tay
    pla
    jsr print_hex
    tya
    jsr print_hex

    ; Print process core.
    lda #@(char-code #\-)
    jsr devcon_print
    lda #0
    sta $9ff4
    ldx current_process
    lda process_cores,x
    jsr print_hex

l:  jmp -l

txt_guru:
    "Guru Meditation.  Meditate and reset." 0
txt_guru_error:
    "Error: " 0
