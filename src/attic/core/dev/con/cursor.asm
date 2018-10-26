devcon_push_cursor_disable:
devcon_pop_cursor_disable:
devcon_draw_cursor:
    pha
    lda devcon_mode
    pha

    lda #DEVCON_MODE_XOR
    sta devcon_mode
    lda #<devcon_gfx_cursor_normal
    sta tmp
    lda #>devcon_gfx_cursor_normal
    sta tmp2
    jsr devcon_blit_char

    pla
    sta devcon_mode
    pla
    rts

devcon_gfx_cursor_normal:
    %1000
    %1000
    %1000
    %1000
    %1000
    %1000
    %1000
    %1000
