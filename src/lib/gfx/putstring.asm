.export putstring

.import putchar
.importzp p

.proc putstring
    ldy #0
    lda (p),y
    beq done
    jsr putchar
    inc p
    bne putstring
    inc p+1
    jmp putstring
done:
    inc p
    bne n
    inc p+1
n:  rts
.endproc
