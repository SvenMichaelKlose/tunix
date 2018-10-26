.import devcon_print

.proc print_hex
    pha
    lsr
    lsr
    lsr
    lsr
    jsr print_nibble
    pla
    and #$0f
    jmp print_nibble
.endproc

.proc print_nibble
    cmp #10
    bcc n
    clc
    adc #'A'-10
    jmp devcon_print
n:  adc #'0'
    jmp devcon_print
.endproc

.proc guru_line
    ldx #screen_columns * 2 - 2
l:  jsr devcon_print
    dex
    bne l
    rts
.endproc

.proc guru_middle
    lda #$ba
    jsr devcon_print
    lda #$20
    jsr guru_line
    lda #$ba
    jmp devcon_print
.endproc

.proc guru_meditation
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
    sta s+1
    jsr devcon_print_string

    ; Get to next line.
    lda #12
    sta xpos
    lda #16
    sta ypos

    lda #<txt_guru_error
    sta s
    lda #>txt_guru_error
    sta s+1
    jsr devcon_print_string

    ; Print error code.
    lda last_error
    jsr print_hex
    lda #$20
    jsr devcon_print

    ; Print current banks.
    ldx #0
l2: lda $9ff4,x
    jsr print_hex
    lda #@(char-code #\:)
    jsr devcon_print
    inx
    inx
    cpx #12
    bne l2

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

w:  jmp w
.endproc

.bss

txt_guru:
    .asciiz "Guru Meditation.  Meditate and reset."
txt_guru_error:
    .asciiz "Error: "
