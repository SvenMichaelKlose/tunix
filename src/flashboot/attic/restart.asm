.proc restart
    lda $9ff2
    and #%00111111
    ora #%01000000
    sta $9ff2
    lda #0
    sta $9ffe
    lda $9fff
    jmp main
.endproc
