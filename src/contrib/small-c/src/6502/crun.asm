.export cguhcar

    .zeropage

hl:
l:  .res 1
h:  .res 1

de:
e:  .res 1
d:  .res 1

    .code

; Load char at primary into primary.
cguchar:
    ldy #0
    lda (hl),y
    sta l
    sty h
    rts
