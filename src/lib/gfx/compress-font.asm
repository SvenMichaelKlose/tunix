.export compress_font

.importzp s, d, tmp, tmp2, font_compression
.code

.proc compress_font
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
    beq n
    sec
    jmp m
n:  clc
m:  ror buffer1,x
    iny
    cpy #8
    bne next_line
    inx
    lsr tmp
    bne next_bit

    ; Clear second buffer.
    ldx #7
    lda #0
l:  sta buffer2,x
    dex
    bpl l

    ; Compress char.
    lda buffer1
    sta buffer2
    lda font_compression
    sta tmp
    ldx #1
    ldy #1
l2: lda buffer1,x
    sta buffer2,y
    cmp buffer2-1,y
    beq n2
j:  iny
    jmp m2
n2: dec tmp
    bmi j
m2: inx
    cpx #8
    bne l2

    ; Turn char +90°.
    lda #1
    sta tmp
    ldx #0
next_bit2:
    ldy #0
next_line2:
    lda buffer2,y
    and tmp
    beq n3
    sec
    jmp m3
n3: clc
m3: rol buffer1,x
    iny
    cpy #8
    bne next_line2
    inx
    asl tmp
    bne next_bit2

    ldy #7
l3: lda buffer1,y
    sta (d),y
    dey
    bpl l3

    lda s
    clc
    adc #8
    sta s
    bcc n4
    inc s+1
n4:

    lda d
    clc
    adc #8
    sta d
    bcc n5
    inc d+1
n5:

    dec tmp2
    beq done
    jmp next_char
done:
    rts
.endproc

.bss
buffer1:    .byte 0, 0, 0, 0, 0, 0, 0, 0
buffer2:    .byte 0, 0, 0, 0, 0, 0, 0, 0
