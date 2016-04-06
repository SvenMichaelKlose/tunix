clear_screen:
    lda #0
    sta xpos
    sta ypos
    lda #<charset
    sta d
    lda #>charset
    sta @(++ d)
    lda #<charset_size
    sta c
    lda #>charset_size
    sta @(++ c)
    jmp clrram
