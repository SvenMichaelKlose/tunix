.export _save_state

.import popax

; void __fastcall__ save_state (unsigned restart, char flags);
; Expects Ultimem regs for return at $120.
.proc _save_state
    sta $0106

    ; Save restart address.
    jsr popax
    sta $0104
    stx $0105

    ; Map ROM in BLK5.
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
    sta $9fff

    jsr $a009

    ; Restore BLK5.
    pla
    sta $9fff
    pla
    sta $9ffe
    pla
    sta $9ff2
    rts
.endproc
