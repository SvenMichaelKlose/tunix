switch_banks_in:
    lda bank_io
    beq +n
    sta $9ff6
n:  lda bank1
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
