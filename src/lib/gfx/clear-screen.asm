.export clear_screen

.importzp s, d, c, xpos, ypos
.import clrram, charset, charset_size

.code

.proc clear_screen
    lda #0
    sta xpos
    sta ypos
    lda #<charset
    sta d
    lda #>charset
    sta d+1
    lda #<charset_size
    sta c
    lda #>charset_size
    sta c+1
    jmp clrram
.endproc
