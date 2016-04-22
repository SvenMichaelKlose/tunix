gfx_init:
    lda #black
    ldx #white
    ldy #cyan
    jsr init_bitmap_mode

    jsr reset_region

    brk
    c_setzb font $88
    c_setzb do_compress_font_gaps 1
    c_setzb font_compression 2
    c_setzb font_space_size 3
    0
    rts
