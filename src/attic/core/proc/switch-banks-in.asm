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
n:  lda saved_bank_ram
    beq +n
    sta $9ff4
n:  rts

save_banks:
    lda $9ff4
    pha
    lda saved_bank1
    beq +n
    lda $9ff8
    sta saved_bank1
n:  lda saved_bank2
    beq +n
    lda $9ffa
    sta saved_bank2
n:  lda saved_bank3
    beq +n
    lda $9ffc
    sta saved_bank3
n:  lda saved_bank5
    beq +n
    sta $9ffe
    sta saved_bank5
n:  lda saved_bank_ram
    beq +n
    pla
    lda $9ff4
    sta saved_bank_ram
    rts

n:  pla
    rts
