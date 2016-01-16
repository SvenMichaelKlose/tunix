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
    jsr compress_font

    lda #0
    sta xpos
    sta ypos
    lda #@(-- screen_width)
    sta width
    lda #@(-- screen_height)
    sta height
    lda #<pat_solid
    sta pattern
    lda #>pat_solid
    sta @(++ pattern)
    jsr frame

    lda #3
    sta xpos
    sta ypos
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
    "A graphical user interface" 0
    "for UltiMem applications." 0
    " " 0
    " " 0
    "Booting..." 0
    0
