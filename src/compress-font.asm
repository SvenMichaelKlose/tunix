compress_font:
    lda #0
    sta tmp2

next_char:
    ; Turn char -90°.
    lda #128
    sta tmp
    ldx #0
next_bit:
    ldy #0
next_line:
    lda (s),y
    and tmp
    beq +n
    sec
    jmp +m
n:  clc
m:  ror buffer1,x
    iny
    cpy #8
    bne -next_line
    inx
    lsr tmp
    bne -next_bit

    ; Clear second buffer.
    ldx #7
    lda #0
l:  sta buffer2,x
    dex
    bpl -l

    ; Compress char.
    lda buffer1
    sta buffer2
    ldx #1
    ldy #1
l:  lda buffer1,x
    sta buffer2,y
    cmp @(-- buffer2),y
    beq +n
    iny
n:  inx
    cpx #8
    bne -l

    ; Turn char +90°.
    lda #1
    sta tmp
    ldx #0
next_bit:
    ldy #0
next_line:
    lda buffer2,y
    and tmp
    beq +n
    sec
    jmp +m
n:  clc
m:  rol buffer1,x
    iny
    cpy #8
    bne -next_line
    inx
    asl tmp
    bne -next_bit

    ldy #7
l:  lda buffer1,y
    sta (d),y
    dey
    bpl -l

    lda s
    clc
    adc #8
    sta s
    bcc +n
    inc @(++ s)
n:

    lda d
    clc
    adc #8
    sta d
    bcc +n
    inc @(++ d)
n:

    dec tmp2
    beq +done
    jmp next_char
done:
    rts

buffer1:    fill 8
buffer2:    fill 8
