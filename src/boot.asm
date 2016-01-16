boot:
    lda #0
    sta $9002
    jsr gfx_init

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

    brk
    c_setpattern <pat_background >pat_background
    c_box 0 0 @(-- screen_width) screen_height
    c_setpattern <pat_empty >pat_empty
    c_box 8 50 @(- screen_width 16) 55
    c_setpattern <pat_solid >pat_solid
    c_apply c_frame
    0

    inc xpos
    inc xpos
    inc ypos
    inc ypos
    inc ypos
    lda #<txt_welcome
    sta p
    lda #>txt_welcome
    sta @(++ p)

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
    jmp boot

txt_welcome:
    "UltiGUI v0.1" 0
    " " 0
    " " 0
    "A graphical user interface for" 0
    "the UltiMem memory expansion." 0
    0
