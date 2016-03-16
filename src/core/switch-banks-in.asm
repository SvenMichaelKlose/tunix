switch_banks_in:
    lda bank1
    ora @(++ bank1)
    beq +n
    lda bank1
    sta $9ff8
    lda @(++ bank1)
    sta $9ff9

n:  lda bank2
    ora @(++ bank2)
    beq +n
    lda bank2
    sta $9ffa
    lda @(++ bank2)
    sta $9ffb

n:  lda bank3
    ora @(++ bank3)
    beq +n
    lda bank3
    sta $9ffc
    lda @(++ bank3)
    sta $9ffd

n:  lda bank5
    ora @(++ bank5)
    beq +n
    lda bank5
    sta $9ffe
    lda @(++ bank5)
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
