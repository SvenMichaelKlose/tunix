clear_screen:
    lda #<charset
    sta d
    lda #>charset
    sta @(++ d)
    lda #<charset_size
    sta c
    lda #>charset_size
    sta @(++ c)
    jmp clrram
