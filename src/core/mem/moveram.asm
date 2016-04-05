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
    bne +k
n:  inc d
    beq +m
q:  dex
    bne -l
    dec @(++ c)
    bne -l
    rts

k:  inc @(++ s)
    jmp -n

m:  inc @(++ d)
    jmp -q

copy_backwards:
l:  lda (s),y
    sta (d),y
    dec s
    lda s
    cmp #$ff
    beq +m
n:  dec d
    lda d
    cmp #$ff
    beq +j
q:  dex
    beq -l
    dec @(++ c)
    bne -l
    rts

m:  dec @(++ s)
    jmp -n

j:  dec @(++ d)
    jmp -q
