.export gfx_init

.import init_bitmap_mode, reset_region, init_region_stack
.importzp c_setzb
.importzp font, font_space_size, font_compression, do_compress_font_gaps, scrbase
.importzp xpos, ypos, xpos2, ypos2, width, height
.import exec_script
.import screen

.code

.proc gfx_init
    lda #<exec_script
    sta $316
    lda #>exec_script
    sta $317

    lda #0
    ldx #1
    ldy #5
    jsr init_bitmap_mode

    jsr init_region_stack
    jsr reset_region

    lda #$00
    sta scrbase
    lda #$11
    sta scrbase+1

    brk
    .byte c_setzb, font, $00
    .byte c_setzb, font+1, $88
    .byte c_setzb, do_compress_font_gaps, 1
    .byte c_setzb, font_compression, 2
    .byte c_setzb, font_space_size, 3
    .byte 0
    rts
.endproc
