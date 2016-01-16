putchar:
    ; ASCII to PETSCII
    cmp #@(++ (char-code #\Z)))
    bcc +n
    sec
    sbc #@(-- #\a)
n:

    ; Get character address.
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
    lda #0
    sta tmp
    lda xpos
    and #7
    sta tmp2
    tax
    lda tab_neg,x
    sta tmp3

    ldy #7
l:  lda (s),y
    tax
    ora tmp
    sta tmp
    txa
    ldx tmp2
    beq +i
m:  lsr
    dex
    bne -m
i:  ora (scr),y
    sta (scr),y
    dey
    bpl -l

    jsr inc_xcpos

    ldy #7
l:  lda (s),y
    ldx tmp3
m:  asl
    dex
    bne -m
    ora (scr),y
    sta (scr),y
    dey
    bpl -l

next_char:
    lda xpos
    clc
    adc #8
    sta xpos
;l:  lsr tmp
;    bcs +done
;    beq +done
;    dec xpos
;    jmp -l
done:
    rts

tab_neg:    0 7 6 5 4 3 2 1

putstring:
    ldy #0
    lda (p),y
    beq done
    jsr putchar
    inc p
    bne putstring
    inc @(++ p)
    jmp putstring
