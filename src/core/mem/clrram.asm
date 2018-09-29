.export clrram

.importzp d, c

.proc clrram
    lda c
    bne +l2
    dec c+1
l2: dec c

    ldy d
    ldx c
    lda #0
    sta d
    sta c
l:  lda #0
    sta (d),y
    iny
    beq m
n:  dex
    cpx #255
    bne l
    dec c+1
    lda c+1
    cmp #255
    bne l
    rts

m:  inc d+1
    jmp n
.endproc
