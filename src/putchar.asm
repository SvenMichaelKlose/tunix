buf_clipped_char:
    fill 8

masks_chars_left:
    %11111111
    %01111111
    %00111111
    %00011111
    %00001111
    %00000111
    %00000011
    %00000001

masks_chars_right:
    %00000000
    %10000000
    %11000000
    %11100000
    %11110000
    %11111000
    %11111100
    %11111110
    %11111111

blit_char:
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
    rts

clip_char:
    ; Copy character into buffer.
    ldy #7
l:  lda (s),y
    sta buf_clipped_char,y
    dey
    bpl -l

    lda #<buf_clipped_char
    sta s
    lda #>buf_clipped_char
    sta @(++ s)

    ; Clip top of character.
    lda ryt
    sec
    sbc ypos
    bcc +n
    tay
    dey
    lda #0
l:  sta buf_clipped_char,y
    dey
    bpl -l
n:

    ; Clip bottom of character.
    lda ypos2
    cmp ryb
    bcc +n
    lda ryb
    sec
    sbc ypos
    tay
    lda #0
l:  sta buf_clipped_char,y
    iny
    cpy #8
    bne -l
n:

    lda #$ff
    sta tmp2

    ; Clip left of character.
    lda rxl
    sec
    sbc xpos
    bcc +n
    tay
    lda masks_chars_left,y
    sta tmp2
n:

    ; Clip right of character.
    lda xpos2
    sec
    sbc rxr
    bcc +n
    tay
    lda masks_chars_right,y
    and tmp2
    sta tmp2
n:

    ; Run mask for sides over character.
    ldy #7
l:  lda tmp2
    and buf_clipped_char,y
    sta buf_clipped_char,y
    dey
    bpl -l
    rts

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

    ; Calculate right bottom point.
    lda xpos
    clc
    adc #8
    sta xpos2
    lda ypos
    clc
    adc #8
    sta ypos2

    ; Skip character if it's outside the visible region.
    lda xpos
    cmp rxr
    bcs +next_char
    lda xpos2
    cmp rxl
    bcc +next_char
    lda ypos
    cmp ryb
    bcs +next_char
    lda ypos2
    cmp ryt
    bcc +next_char

    ; Check if character needs to get clipped.
    lda xpos
    cmp rxl
    bcc +m
    lda rxr
    cmp xpos2
    bcc +m
    lda ypos
    cmp ryt
    bcc +m
    lda ryb
    cmp ypos2
    bcs +n
m:  jsr clip_char
n:

    jsr calcscr
    jsr blit_char

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

    ; Leave 1 pixel gap.
done:
    inc xpos
    rts

    ; Add default gap for spaces.
n:  lda xpos
    sec
    sbc #8
    clc
    adc font_space_size
    sta xpos
j:  rts

tab_neg:    0 7 6 5 4 3 2 1
