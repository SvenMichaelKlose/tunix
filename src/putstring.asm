putstring:
    ldy #0
    lda (p),y
    beq +done
    jsr putchar
    inc p
    bne putstring
    inc @(++ p)
    jmp putstring
done:
    inc p
    bne +n
    inc @(++ p)
n:  rts
