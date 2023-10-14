.export clrram

.importzp d, c

.proc clrram
    ldx c
    inx
    inc c+1
    ldy d
    lda #0
    sta d
    bne +n ; (jmp)
l:  sta (d),y
    iny
    beq m
n:  dex
    bne l
    dec c+1
    bne l
    rts
m:  inc d+1
    jmp n
.endproc
