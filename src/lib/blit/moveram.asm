.export moveram

.importzp s, d, c

.code

.proc moveram   ; TODO: Rename.
    ldy #0
    ldx c
    inx
    inc c+1
    cmp #0
    beq copy_forwards
    bne copy_backwards

l:  lda (s),y
    sta (d),y
    iny
    beq k
copy_forwards:
q:  dex
    bne l
    dec c+1
    bne l
r:  rts
k:  inc s+1
    inc d+1
    jmp q

l2: lda (s),y
    sta (d),y
    dey
    cpy #$ff
    beq m2
copy_backwards:
q2: dex
    bne l2
    dec c+1
    bne l2
    rts
m2: dec s+1
    dec d+1
    jmp q2
.endproc
