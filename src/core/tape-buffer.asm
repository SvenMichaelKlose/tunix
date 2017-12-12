library_core: 0

switch:
    pha

    ;; Restart NMI.
    lda #$80
    sta $9115

    ;; Return if multitasking has been turned off.
    lda takeovers
    bne +n
    sta needs_switch
    pla
    rti

n:  jmp switch2

return_from_switch:
    lda saved_bank5
    sta $9ffe
    lda saved_bank_ram
    sta $9ff4
    lda saved_a
    jsr release
    rti

library_return:
    pla
    sta $9ff4
    php
    jsr switch_banks_in
    plp
    rts

library_call:
    sta library_core
    lda $9ff4
    pha
    lda #@(high (-- library_return))
    pha
    lda #@(low (-- library_return))
    pha
    tya
    pha
    txa
    pha
    lda current_process
    sta $9ff4
    jsr save_banks
    lda library_core
    sta $9ff4

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
