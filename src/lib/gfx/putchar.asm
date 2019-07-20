.export putchar

.import inc_xcpos, calcscr, add_region_position, sub_region_position
.import masks_left, masks_right
.importzp tmp, tmp2, tmp3
.importzp xpos, ypos, xpos2, ypos2, s, scr, ryb, ryt, rxl, rxr
.importzp font, font_bank, font_space_size, do_compress_font_gaps, pencil_mode

.bss

buf_clipped_char:
    .byte 0, 0, 0, 0, 0, 0, 0, 0

.data

tab_neg:
    .byte 0, 7, 6, 5, 4, 3, 2, 1

.code

.proc blit_char
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
    beq i
m:  lsr
    dex
    bne m
i:  ora (scr),y
    sta (scr),y
    dey
    bpl l

    ; Step to next column.
    jsr inc_xcpos

    ; Draw optional right column.
    ldy #7
l2: lda (s),y
    ldx tmp3
m2: asl
    dex
    bne m2
    ora (scr),y
    sta (scr),y
    dey
    bpl l2
    rts
.endproc

.proc clip_char
    ; Copy character into buffer.
    ldy #7
l3: lda (s),y
    sta buf_clipped_char,y
    dey
    bpl l3

    lda #<buf_clipped_char
    sta s
    lda #>buf_clipped_char
    sta s+1

    ; Clip top of character.
    lda ryt
    sec
    sbc ypos
    bcc n4
    tay
    dey
    lda #0
l4: sta buf_clipped_char,y
    dey
    bpl l4
n4:

    ; Clip bottom of character.
    lda ypos2
    cmp ryb
    bcc n5
    lda ryb
    sec
    sbc ypos
    tay
    lda #0
l5: sta buf_clipped_char,y
    iny
    cpy #8
    bne l5
n5:

    lda #$ff
    sta tmp2

    ; Clip left of character.
    lda rxl
    sec
    sbc xpos
    bcc n6
    tay
    lda masks_left,y
    sta tmp2
n6:

    ; Clip right of character.
    lda xpos2
    sec
    sbc rxr
    bcc n7
    tay
    lda masks_right,y
    and tmp2
    sta tmp2
n7:

    ; Run mask for sides over character.
    ldy #7
l6: lda tmp2
    and buf_clipped_char,y
    sta buf_clipped_char,y
    dey
    bpl l6
    rts
.endproc

.proc ascii2petscii
    cmp #'Z'+1
    bcc n
    sec
    sbc #'a'-1
n:  rts
.endproc

.proc putchar
    pha     ; Remove?
    jsr add_region_position
    pla
    sta tmp
    lda $9ff2
    pha
    and #%00111111
    ora #%11000000
    sta $9ff2
    lda $9ffe
    pha
    lda $9fff
    pha
    lda font_bank
    sta $9ffe
    lda #0
    sta $9fff
    lda tmp

    ; Get character address.
    asl
    adc #0
    asl
    adc #0
    asl
    adc #0
    tay
    and #%11111000
    clc
    adc font
    sta s
    tya
    and #%00000111
    adc font+1
    sta s+1

    ; OR all lines together to find the paddings left and right.
    ldy #7
    lda #0
l:  ora (s),y
    dey
    bpl l
    sta tmp

    ; Check if the left gap should be removed.
    ldx xpos    ; XXX really needed?
    beq n2
    ldx do_compress_font_gaps
    beq n2

    ; Move the character left to remove the gap.
l2: asl
    bcs n2
    beq n2
    dec xpos
    jmp l2
n2:

    ; Calculate right bottom point.
    lda xpos
    clc
    adc #8
    sta xpos2
    lda ypos
    clc
    adc #8
    sta ypos2

    ; Don't clip or draw if pencil mode is 0.
    lda pencil_mode
    beq next_char

    ; Skip character if it's outside the visible region.
    lda xpos
    cmp rxr
    bcs next_char
    lda xpos2
    cmp rxl
    bcc next_char
    lda ypos
    cmp ryb
    bcs next_char
    lda ypos2
    cmp ryt
    bcc next_char

    ; Check if character needs to get clipped.
    lda xpos
    cmp rxl
    bcc m
    lda rxr
    cmp xpos2
    bcc m
    lda ypos
    cmp ryt
    bcc m
    lda ryb
    cmp ypos2
    bcs n3
m:  jsr clip_char
n3:

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
    beq j
    lda tmp
    beq n4      ; Add default gap for spaces…

    ; Move to pointer to the left to close the gap.
    lda tmp
l3: lsr
    bcs done
    beq done
    dec xpos
    jmp l3

    ; Leave 1 pixel gap.
done:
    inc xpos
    jmp j

    ; Add default gap for spaces.
n4: lda xpos
    sec
    sbc #8
    clc
    adc font_space_size
    sta xpos
j:  
    pla
    sta $9fff
    pla
    sta $9ffe
    pla
    sta $9ff2
    jmp sub_region_position
.endproc
