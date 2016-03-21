compare_asciiz:
    lda (s),y
    cmp (d),y
    bne +done
    iny
    cmp #0
    bne compare_asciiz
done:
    rts
