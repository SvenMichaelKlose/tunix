moveram:
    ldy #0
    ldx c
    sty c
    inc @(++ c)
    cmp #0
    bne copy_backwards

copy_forwards:
l:  lda (s),y
    sta (d),y
    inc s
    bne +n
    inc @(++ s)
n:  inc d
    bne +n
    inc @(++ d)
n:  dex
    bne -l
    dec @(++ c)
    bne -l
    rts

copy_backwards:
l:  lda (s),y
    sta (d),y
    dec s
    lda s
    cmp #$ff
    bne +n
    dec @(++ s)
n:  dec d
    lda d
    cmp #$ff
    bne +n
    dec @(++ d)
n:  dex
    bne -l
    dec @(++ c)
    bne -l
    rts
