.export ultimem_read_byte
.export ultimem_write_byte
.export ultimem_get_bank
.export ultimem_unhide_regs
.exportzp bp
.importzp s, d, c, tmp

.zeropage

bp:     .res 4

.code

.proc ultimem_get_bank
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

; Fetch byte from Flash memory at 24-bit offset in 0,X.
.proc ultimem_read_byte
    tya
    pha
    lda $9ff2
    pha
    lda #%01111101
    sta $9ff2
    lda $9ff8
    pha
    lda $9ff9
    pha

    ldy #8
    jsr ultimem_get_bank
    lda s+1
    ora #$20
    sta s+1
    lda (s),y
    sta tmp

    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff2
    pla
    tay
    lda tmp
    rts
.endproc

; Write byte to RAM at 24-bit offset in 0,X.
.proc ultimem_write_byte
    sta tmp
    tya
    pha
    lda $9ff2
    pha
    lda #%01111111
    sta $9ff2
    lda $9ff8
    pha
    lda $9ff9
    pha

    ldy #8
    jsr ultimem_get_bank
    lda s+1
    ora #$20
    sta s+1
    lda tmp
    sta (s),y

    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff2
    pla
    tay
    rts
.endproc

; Returns ID.
.proc ultimem_unhide_regs
    lda $9f55
    lda $9faa
    lda $9f01
    lda $9ff3
.endproc
