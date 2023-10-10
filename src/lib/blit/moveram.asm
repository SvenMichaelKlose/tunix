.export moveram

.importzp s, d, c

.code

.proc moveram
    ldy #0
    ldx c
    cmp #0
    bne copy_backwards

copy_forwards:
l:  lda (s),y
    sta (d),y
    iny
    beq k
q:  dex
    bne l
    lda c+1
    beq r
    dec c+1
    bne l
r:  rts
k:  inc s+1
    inc d+1
    jmp q

copy_backwards:
l2: lda (s),y
    sta (d),y
    dey
    cpy #$ff
    beq m2
q2: dex
    bne l2
    lda c+1
    beq r
    dec c+1
    bne l2
    rts
m2: dec s+1
    dec d+1
    jmp q2
.endproc
