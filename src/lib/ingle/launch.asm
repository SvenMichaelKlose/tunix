.export _launch
.export _ingle_exec
.import popax

; void __fastcall__ launch (unsigned long offset, unsigned start, unsigned size);
.proc _launch
    sta $08     ; c
    stx $09
    jsr popax
    sta $04     ; d
    stx $05
    jsr popax   ; s
    sta $00
    stx $01
    jsr popax
    sta $02
    stx $03

    lda $9ff2
    and #%00111111
    ora #%01000000
    sta $9ff2
    lda #0
    sta $9ffe
    sta $9fff
    
    jmp $a00c
.endproc

; void __fastcall__ ingle_exec (unsigned long offset, unsigned start, unsigned size);
.proc _ingle_exec
    sta $08     ; c
    stx $09
    jsr popax
    sta $04     ; d
    stx $05
    jsr popax   ; s
    sta $00
    stx $01
    jsr popax
    sta $02
    stx $03

    lda $9ff2
    and #%00111111
    ora #%01000000
    sta $9ff2
    lda #0
    sta $9ffe
    sta $9fff

    jmp $a018
.endproc
