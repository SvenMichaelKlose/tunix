putchar:
    cmp #@(++ (char-code #\Z)))   ; ASCII to PETSCII
    bcc +n
    sec
    sbc #@(-- #\a)
n:

    asl
    adc #0
    asl
    adc #0
    asl
    adc #0
    tay
    and #%11111000
    sta s
    tya
    and #%00000111
    ora #$88
    sta @(++ s)

    jsr calcscr

    ldy #7
l:  lda (s),y
    sta (scr),y
    dey
    bpl -l

    lda xpos
    clc
    adc #8
    sta xpos
done:
    rts

putstring:
    ldy #0
    lda (p),y
    beq done
    jsr putchar
    inc p
    bne putstring
    inc @(++ p)
    jmp putstring
