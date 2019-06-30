.export _launch
.import popax

; void __fastcall__ launch (unsigned start, unsigned size);
.proc _launch
    sta $0d     ; c
    stx $0e
    jsr popax
    sta $09     ; d
    stx $0a

    lda $9ff2
    and #%00111111
    ora #%01000000
    sta $9ff2
    lda #0
    sta $9ffe
    lda $9fff
    
    jmp $a00c
.endproc
