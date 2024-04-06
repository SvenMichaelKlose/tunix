    .segment "RAMIFY"

    sei
    lda #$7f
    sta $911d
    sta $911e
    cld
    ldx #$ff
    txs

    jsr _ultimem_unhide

    lda #%01110000
    sta $9ff2
    ldx #0
l:  lda ramify,x
    sta $9800,x
    dex
    bne l
    lda #%11111111
    sta $9ff2
    jmp $9800

ramify:
    lda #0
    sta s
    sta d
    sta c
    lda #$a0
    sta s+1
    lda #$60
    sta d+1
    lda #$20
    sta c+1
    lda #8
    sta $9ffc
    sta s
    sta d
    sta c

    ldy #0
    ldx c
    inx
    inc c+1
    bne copy_forwards ; (jmp)

l:  lda (s),y
    sta (d),y
    iny
    beq k
copy_forwards:
q:  dex
    bne l
    dec c+1
    bne l
r:  lda #%11111111
    sta $9ff2
    lda #8
    sta $9ffe
    jmp continue
k:  inc s+1
    inc d+1
    jmp q
.endproc

continue:
