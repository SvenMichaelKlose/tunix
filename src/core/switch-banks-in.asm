switch_banks_in:
    lda bank1
    beq +n
    sta $9ff8
n:  lda bank2
    beq +n
    sta $9ffa
n:  lda bank3
    beq +n
    sta $9ffc
n:  lda bank5
    beq +n
    sta $9ffe
n:  rts

return_to_process:
    pla
    sta $9ffe
    pla
    sta $9ffc
    pla
    sta $9ffa
    pla
    sta $9ff8
    pla
    sta $9ff4
    rts
