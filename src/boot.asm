boot:
    lda #0
    sta $9002
    jsr gfx_init

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
    lda #1
    sta do_compress_font_gaps

    lda #0
    sta xpos
    sta ypos
    lda #@(-- screen_width)
    sta width
    lda #@(-- screen_height)
    sta height
    lda #<pat_background
    sta pattern
    lda #>pat_background
    sta @(++ pattern)
    jsr box

    lda #8
    sta xpos
    lda #50
    sta ypos
    lda #@(- screen_width 16)
    sta width
    lda #@55
    sta height
    lda #<pat_empty
    sta pattern
    lda #>pat_empty
    sta @(++ pattern)
    jsr box
    lda #<pat_solid
    sta pattern
    lda #>pat_solid
    sta @(++ pattern)
    jsr frame

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
