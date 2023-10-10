.export clrram

.importzp d, c

.proc clrram
    ldx c
    ldy d
    lda #0
    sta d
    inc c+1
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
