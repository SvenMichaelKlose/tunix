.export clear_screen
.import s, d, c
.import xpos, ypos, charset, charset_size
.import clrram
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
