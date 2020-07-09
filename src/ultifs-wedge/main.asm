.export _main

.forceimport __STARTUP__


.data

txt_welcome:
    .byte $93, "ULTIFS WEDGE", 13, 0


.code

.proc _fook
    rts
.endproc

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e

    rts
.endproc
