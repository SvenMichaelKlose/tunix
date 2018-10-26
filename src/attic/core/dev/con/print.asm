DEVCON_MODE_OR = 0
DEVCON_MODE_XOR = 1

devcon_print_string:
    ldy #0
l:  lda (s),y
    beq +n
    jsr devcon_print_ctrl
    jsr inc_s
    jmp -l

n:  rts

devcon_print_return:
    jsr devcon_pop_cursor_disable

    pla
    tay
    pla
    tax
    pla
    rts

devcon_print_ctrl:
    sta tmp3
    pha
    txa
    pha
    tya
    pha

    jsr devcon_push_cursor_disable

    lda tmp3
    beq -devcon_print_return
    cmp #10
    bne +l
    jmp next_line

devcon_print:
    sta tmp3
    pha
    txa
    pha
    tya
    pha

    jsr devcon_push_cursor_disable

l:  lda tmp3
    jsr devcon_draw_char

    inc devcon_cursor_x
    lda devcon_cursor_x
    cmp #@(/ screen_width 4)
    bne +n

next_line:
    lda #0
    sta devcon_cursor_x

    lda devcon_cursor_y
    cmp #@(-- (/ screen_height 8))
    bne +l
    jsr devcon_scroll_up
    jmp +n

l:  inc devcon_cursor_y
n:  jmp devcon_print_return

devcon_scroll_up:
    lda s
    pha
    lda @(++ s)
    pha

    lda #@(low (+ charset 8))
    sta s
    lda #<charset
    sta d
    lda #>charset
    sta @(++ s)
    sta @(++ d)
    lda #@(low (- charset_size 8))
    sta c
    lda #@(high (- charset_size 8))
    sta @(++ c)
    lda #0
    jsr moveram

last_screen_row = @(+ charset (- screen_height 8))
    lda #<last_screen_row
    sta s
    lda #>last_screen_row
    sta @(++ s)

    ldx #screen_columns
m:  ldy #7
    lda #0
l:  sta (s),y
    dey
    bpl -l

    lda s
    clc
    adc #screen_height
    sta s
    bcc +n
    inc @(++ s)
n:  dex
    bne -m

    pla
    sta @(++ s)
    pla
    sta s

    rts

devcon_draw_char:
    ldy #0
    sty tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    sta tmp
    lda tmp2
    ora #$20
    sta tmp2

    lda $9ff8
    pha
    lda #BANK_DEVCON_CHARSET
    sta $9ff8
    jsr devcon_blit_char
    pla
    sta $9ff8

    rts

devcon_blit_char:
    lda devcon_cursor_x
    asl
    asl
    sta xpos
    lda devcon_cursor_y
    asl
    asl
    asl
    sta ypos
    jsr calcscr

    lda devcon_mode
    bne xor_mode

    lda devcon_cursor_x
    lsr
    bcs +n

    ldy #7
l:  lda (tmp),y
    asl
    asl
    asl
    asl
    ora (scr),y
    sta (scr),y
    dey
    bpl -l
    bmi +m

n:  ldy #7
l:  lda (tmp),y
    ora (scr),y
    sta (scr),y
    dey
    bpl -l
    bmi +m

xor_mode:
    lda devcon_cursor_x
    lsr
    bcs +n

    ldy #7
l:  lda (tmp),y
    asl
    asl
    asl
    asl
    eor (scr),y
    sta (scr),y
    dey
    bpl -l
    bmi +m

n:  ldy #7
l:  lda (tmp),y
    eor (scr),y
    sta (scr),y
    dey
    bpl -l
m:

    rts
