.export _launch
.import popax

; void __fastcall__ launch (unsigned start, unsigned size);
.proc _launch
    sta $08     ; c
    stx $09
    jsr popax
    sta $04     ; d
    stx $05

    lda $9ff2
    and #%00111111
    ora #%01000000
    sta $9ff2
    lda #0
    sta $9ffe
    lda $9fff
    
    jmp $a00c
.endproc
