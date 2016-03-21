print:
    stx s
    sty @(++ s)
l:  ldy #0
    lda (s),y
    beq +done
    jsr take_over   ; Stop multitasking.
    jsr $ffd2       ; Print character via KERNAL.
    jsr release     ; Resume multitasking.
    jsr inc_s
    jmp -l
done:
    rts
