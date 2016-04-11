switch_banks_in:
    lda saved_bank1
    beq +n
    sta $9ff8
n:  lda saved_bank2
    beq +n
    sta $9ffa
n:  lda saved_bank3
    beq +n
    sta $9ffc
n:  lda saved_bank5
    beq +n
    sta $9ffe
n:  rts
