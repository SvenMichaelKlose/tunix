.export ultimem_offset2bank
.export ultimem_bank2offset

.importzp s, d, c, tmp

.code

; Set bank register at $9ff0+Y to match offset at
; zero page address X.
.proc ultimem_offset2bank
    lda 1,x
    lsr
    lsr
    lsr
    lsr
    lsr
    sta $9ff0,y
    lda 2,x
    asl
    asl
    asl
    ora $9ff0,y
    sta $9ff0,y
    lda 2,x
    lsr
    lsr
    lsr
    lsr
    lsr
    sta $9ff1,y
    lda 0,x
    sta s
    lda 1,x
    and #%00011111
    sta s+1
    ldy #0
    rts
.endproc

; Get offset of bank # in A to 0,X
.proc ultimem_bank2offset
    ldy #0
    sty 0,x
    sty 1,x
    sty 2,x
    sty 3,x
    ldy #5
l1: asl
    rol 2,x
    dey
    bne l1
    sta 1,x
    rts
.endproc
