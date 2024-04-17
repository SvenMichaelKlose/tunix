.importzp s, d, sh, dh, cl, ch

.export cmpmem
.proc cmpmem
    ldx cl
    inx
    inc ch
    ldy #0
    beq :+ ; (jmp)
l:  lda (s),y
    cmp (d),y
    bne r
    iny
    beq y0
:   dex
    bne l
    dec ch
    bne l
r:  rts
y0: inc sh
    inc dh                              
    bne :- ; (jmp)
.endproc
