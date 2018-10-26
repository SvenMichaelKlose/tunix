strcmp:
    ldy #0
l:  lda (s),y
    cmp (d),y
    bne +done
    iny
    cmp #0
    bne -l
done:
    rts
