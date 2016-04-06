devcon_print_string:
    ldy #0
l:  lda (s),y
    beq +done
    jsr devcon_print_ctrl
    jsr inc_s
    jmp -l

done:
    rts

done:
    pla
    tay
    pla
    tax
    rts

devcon_print_ctrl:
    sta tmp2
    txa
    pha
    tya
    pha

    lda tmp2
    beq -done
    cmp #10
    bne +l
    jmp next_line

devcon_print:
    sta tmp2
    txa
    pha
    tya
    pha

l:  lda tmp2
    ldy #0
    sty tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    asl
    rol tmp2
    clc
    adc #<charset_4x8
    sta tmp
    lda tmp2
    adc #>charset_4x8
    sta tmp2
    
    jsr calcscr

    lda xpos
    and #4
    bne +n

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

m:  lda xpos
    clc
    adc #4
    sta xpos
    cmp #screen_width
    bne +n

next_line:
    lda #0
    sta xpos
    lda ypos
    clc
    adc #8
    sta ypos

    cmp #screen_height
    bne +n
    jsr devcon_scroll_up
    lda #@(- screen_height 8)
    sta ypos
n:  
    jmp -done

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
    ldy #7
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
    bne -l

    pla
    sta @(++ s)
    pla
    sta s

    rts
