.export _main
.importzp s

.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e

    rts
.endproc

txt_welcome:
    .byte $93, "ULTIFS WEDGE", 13, 0
