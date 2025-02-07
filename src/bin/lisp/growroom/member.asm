(proc member
    ldy #0
    lda (arg2),y
    lsr
    bcs done    ; Atom...
    ldy #(++ cons.car)
    lda (arg2),y
    cmp (++ arg1)
    bne next
    tax
    dey
    lda (arg2),y
    cmp arg1
    bne next
    lda arg2
    ldx (++ arg2)
    rts
next:
    ldy #(++ cons.cdr)
    lda (arg2),y
    tax
    dey
    lda (arg2),y
    sta arg2
    stx (++ arg2)
    jmp member
done:
    lda #<nil
    ldx #>nil
    rts)
