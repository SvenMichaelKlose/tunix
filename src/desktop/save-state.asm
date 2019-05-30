.export _save_state

; void __fastcall__ save_state (unsigned restart);
.proc _save_state
    tay
    lda $9ff2
    pha
    and #%00111111
    ora #%01000000
    sta $9ff2
    lda $9ffe
    pha
    lda $9fff
    pha
    lda #0
    sta $9ffe
    lda $9fff
    jsr l1
    pla
    sta $9fff
    pla
    sta $9ffe
    pla
    sta $9ff2
    rts

l1: jmp ($a009)
.endproc
