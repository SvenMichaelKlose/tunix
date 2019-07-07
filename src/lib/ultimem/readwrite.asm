.export ultimem_read_byte
.export ultimem_write_byte

.importzp s, d, c, tmp

.import ultimem_offset2bank

.code

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
    jsr ultimem_offset2bank
    lda s+1
    ora #$20
    sta s+1
    ldy #0
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
    jsr ultimem_offset2bank
    lda s+1
    ora #$20
    sta s+1
    ldy #0
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
