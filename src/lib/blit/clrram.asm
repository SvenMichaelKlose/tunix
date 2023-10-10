.export clrram

.importzp d, c

.proc clrram
    ldx c
    ldy d
    lda #0
    sta d
l:  lda #0
l2: sta (d),y
    iny
    beq m
n:  dex
    bne l2
    lda c+1
    beq r
    dec c+1
    bne l ; (jmp)
r:  rts
m:  inc d+1
    jmp n
.endproc
