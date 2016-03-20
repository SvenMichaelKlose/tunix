print:
    stx s
    sty @(++ s)
l:  ldy #0
    lda (s),y
    beq +done
    jsr $ffd2
    jsr inc_s
    jmp -l
done:
    rts
