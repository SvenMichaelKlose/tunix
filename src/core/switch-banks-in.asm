switch_banks_in:
    lda bank_1
    ora @(++ bank_1)
    beq +n
    lda bank_1
    sta $9ff8
    lda @(++ bank_1)
    sta $9ff9

    lda bank_2
    ora @(++ bank_2)
    beq +n
    lda bank_2
    sta $9ffa
    lda @(++ bank_2)
    sta $9ffb

    lda bank_3
    ora @(++ bank_3)
    beq +n
    lda bank_3
    sta $9ffc
    lda @(++ bank_3)
    sta $9ffd

    lda bank_5
    ora @(++ bank_5)
    beq +n
    lda bank_5
    sta $9ffe
    lda @(++ bank_5)
    sta $9fff

    rts

return_from_process:
    ; Map master core to $2000.
    lda #0
    sta $9ff8
    sta $9ff9
    jmp @(+ #x2000 (- #x0400 +l))

    ; Map in core on stack.
l:  pla
    sta $9ff5
    pla
    sta $9ff4
    jmp @(+ #x2000 (- #x0400 +l))

l:  pla
    sta $9ffb
    pla
    sta $9ffa
    pla
    sta $9ff9
    pla
    sta $9ff8
    rts
