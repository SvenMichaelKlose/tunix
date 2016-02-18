boot:
    lda #<exec_script
    sta $316
    lda #>exec_script
    sta $317

    brk
    c_setzw s $00 $88
    c_setzw d $00 $30
    c_setzb font_compression 2
    0
    jsr compress_font

    jsr gfx_init

redraw:
    brk
    c_setpattern <pat_background >pat_background
    c_box 0 0 @(-- screen_width) screen_height
    c_setpattern <pat_empty >pat_empty
    c_box 8 4 @(- screen_width 16) 119
    c_setpattern <pat_solid >pat_solid
    c_apply c_frame
    c_addx 2
    c_addy 3
    c_setzw p <txt_welcome >txt_welcome
    0

l:  ldy #0
    lda (p),y
    beq +done
    lda xpos
    pha
    lda ypos
    pha
    jsr putstring
    lda #$30
    sta font
    pla
    adc #10
    sta ypos
    pla
    sta xpos
    jmp -l

done:
    brk
    c_setzb xpos 80
    c_setzb ypos 120
    c_setzb width 60
    c_setzb height 50
    0
    jsr window

l:  jsr $ffe4
    beq -l
    jmp redraw

txt_welcome:
    "Commodore VIC-20 GUI" 0
    " " 0
    " " 0
    "Graphics primitives for over-" 0
    "lapping windows and dynamic" 0
    "font rendering in less than 2K." 0
    " " 0
    "This font is generated from" 0
    "the system font at startup." 0
    " " 0
    "All drawing is clipped." 0
    0
