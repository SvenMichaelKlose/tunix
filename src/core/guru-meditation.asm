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

guru_meditation:
    lda #%00001000
    sta $900f
    jsr clear_screen
    lda #red
    jsr fill_colors

    lda #$c9
    jsr devcon_print
    lda #$cd
    jsr guru_line
    lda #$bb
    jsr devcon_print
    lda #$ba
    jsr devcon_print
    lda #$20
    jsr guru_line
    lda #$ba
    jsr devcon_print
    lda #$c8
    jsr devcon_print
    lda #$cd
    jsr guru_line
    lda #$bc
    jsr devcon_print

    ; Print guru message.
    lda #8
    sta xpos
    sta ypos
    lda #<txt_guru
    sta s
    lda #>txt_guru
    sta @(++ s)
    jsr devcon_print_string

    ; Print process ID.
    lda $9ff4
    jsr print_hex

    lda #@(char-code #\:)
    jsr devcon_print

    ; Print callee's address.
    pla
    tay
    pla
    jsr print_hex
    tya
    jsr print_hex
l:  jmp -l

txt_guru:
    "Guru Meditation at " 0
