.export ultimem_copy_rom2ram
.export ultimem_copy_ram2rom
.importzp s, d, c, base, size, ptr

.import ultimem_get_bank

.code

.proc copyd
    lda 0,x
    sta 0,y
    lda 1,x
    sta 1,y
    lda 2,x
    sta 2,y
    lda 3,x
    sta 3,y
    rts
.endproc

.proc ultimem_copy_rom2ram
    lda $9ff2
    pha
    lda $9ff8
    pha
    lda $9ff9
    pha
    lda $9ffa
    pha
    lda $9ffb
    pha

    lda #%01111101  ; ROMRAMRAMROM…
    sta $9ff2

    ldx #ptr
    ldy #base
    jsr copyd

    ldx #base
    ldy #$08
    jsr ultimem_get_bank
    lda s
    sta base
    lda s+1
    ora #$20
    sta base+1

    ldx #d
    ldy #$0a
    jsr ultimem_get_bank
    lda s
    sta d
    lda s+1
    ora #$40
    sta d+1

l3: ldy #0
    lda (base),y
    sta (d),y
    inc base
    bne l4
    inc base+1
    lda base+1
    cmp #$40
    bne l4
    lda #$20
    sta base+1
    inc $9ff8
    bne l4
    inc $9ff9

l4: inc d
    bne l5
    inc d+1
    lda d+1
    cmp #$60
    bne l5
    lda #$40
    sta d+1
    inc $9ffa
    bne l5
    inc $9ffb

l5: dec size
    lda size
    cmp #255
    bne l3
    dec size+1
    lda size+1
    cmp #255
    bne l3

    pla
    sta $9ffb
    pla
    sta $9ffa
    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff2
    rts
.endproc

.proc ultimem_copy_ram2rom
    lda $9ff2
    pha
    lda $9ff8
    pha
    lda $9ff9
    pha
    lda $9ffa
    pha
    lda $9ffb
    pha

    lda #%01110101  ; ROMRAMROMROM…
    sta $9ff2

    ldx #ptr
    ldy #base
    jsr copyd

    ldx #base
    ldy #$08
    jsr ultimem_get_bank
    lda s
    sta base
    lda s+1
    ora #$20
    sta base+1

    ldx #d
    ldy #$0a
    jsr ultimem_get_bank
    lda s
    sta d
    lda s+1
    ora #$40
    sta d+1

l3: ldy #0
    lda (base),y
    sta (d),y
    inc base
    bne l4
    inc base+1
    lda base+1
    cmp #$40
    bne l4
    lda #$20
    sta base+1
    inc $9ff8
    bne l4
    inc $9ff9

l4: inc d
    bne l5
    inc d+1
    lda d+1
    cmp #$60
    bne l5
    lda #$40
    sta d+1
    inc $9ffa
    bne l5
    inc $9ffb

l5: dec size
    lda size
    cmp #255
    bne l3
    dec size+1
    lda size+1
    cmp #255
    bne l3

    pla
    sta $9ffb
    pla
    sta $9ffa
    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff2
    rts
.endproc
