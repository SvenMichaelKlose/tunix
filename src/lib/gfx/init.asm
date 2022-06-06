.export gfx_init

.import init_bitmap_mode, reset_region, init_region_stack
.importzp c_setzb
.importzp font, font_space_size, font_compression, do_compress_font_gaps, scrbase
.importzp xpos, ypos, xpos2, ypos2, width, height
.importzp pencil_mode
.import exec_script
.import screen

.code

.proc gfx_init
    jsr init_bitmap_mode

    jsr init_region_stack
    jsr reset_region

    lda #$00
    sta scrbase
    lda #$11
    sta scrbase+1

    ldy #0
    sty font
    iny
    sty pencil_mode
    sty do_compress_font_gaps
    iny
    sty font_compression
    iny
    sty font_space_size
    ldy #88
    sty font+1
    rts
.endproc
