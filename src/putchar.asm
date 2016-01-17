putchar:
    pha
    lda xpos
    clc
    adc #8
    sta xpos2
    lda ypos
    clc
    adc #8
    sta ypos2
    pla

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
    ora font
    sta @(++ s)

    ; OR all line together to find the paddings left and right.
    ldy #7
    lda #0
l:  ora (s),y
    dey
    bpl -l
    sta tmp

    ; Check if the left gap should be removed.
    ldx xpos    ; XXX really needed?
    beq +n
    ldx do_compress_font_gaps
    beq +n

    ; Move the character left to remove the gap.
l:  asl
    bcs +n
    beq +n
    dec xpos
    jmp -l
n:

    jsr calcscr

    ; Calculate sub–column shifts.
    lda xpos
    and #7
    sta tmp2
    tax
    lda tab_neg,x
    sta tmp3

    ; Draw left (or only) column.
    ldy #7
l:  lda (s),y
    ldx tmp2
    beq +i
m:  lsr
    dex
    bne -m
i:  ora (scr),y
    sta (scr),y
    dey
    bpl -l

    ; Step to next column.
    jsr inc_xcpos

    ; Draw optional right column.
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

    ; Update position to next character.
next_char:
    ; Do a standard character width jump to the right.
    lda xpos
    clc
    adc #8
    sta xpos

    ; Check if we should remove the right gap.
    ldx do_compress_font_gaps
    beq +j
    lda tmp
    beq +n      ; Add default gap for spaces…

    ; Move to pointer to the left to close the gap.
    lda tmp
l:  lsr
    bcs +done
    beq +done
    dec xpos
    jmp -l

    ; Add default gap for spaces.
n:  lda xpos
    sec
    sbc #8
    clc
    adc font_space_size
    sta xpos
j:  rts

    ; Leave 1 pixel gap.
done:
    inc xpos
    rts

tab_neg:    0 7 6 5 4 3 2 1
