boot:
    lda #<exec_script
    sta $316
    lda #>exec_script
    sta $317

    lda #$00
    sta s
    sta d
    lda #$88
    sta @(++ s)
    lda #$30
    sta @(++ d)
    lda #2
    sta font_compression
    jsr compress_font

    jsr gfx_init

redraw:
    brk
    c_setpattern <pat_background >pat_background
    c_box 0 0 @(-- screen_width) screen_height
    c_setpattern <pat_empty >pat_empty
    c_box 8 50 @(- screen_width 16) 55
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
    lda #70
    sta xpos
    lda #20
    sta ypos
    lda #60
    sta width
    lda #100
    sta height
    jsr window

l:  jsr $ffe4
    beq -l
    jmp redraw

txt_welcome:
    "UltiGUI v0.1" 0
    " " 0
    " " 0
    "A graphical user interface for" 0
    "the UltiMem memory expansion." 0
    0
