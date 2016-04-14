devcon_keyboard_vops:
    <devcon_read_keyboard >devcon_read_keyboard ; read
    <devcon_error >devcon_error ; write
    <devcon_error >devcon_error ; lookup

devcon_screen_vops:
    <devcon_error >devcon_error ; read
    <devcon_write_screen >devcon_write_screen ; write
    <devcon_error >devcon_error ; lookup

devcon_error:
    lda #ENOSYS
    jmp set_error

devcon_init:
    lda #green
    ldx #black
    ldy #black
    jsr init_bitmap_mode
    jsr devcon_clear_screen
    lda #DEVCON_MODE_OR
    sta devcon_mode

    lda #FILE_OPENED
    sta vfile_states
    sta @(++ vfile_states)
    sta @(+ 2 vfile_states)
    ldy #0
    sty vfile_handles
    iny
    sty @(++ vfile_handles)
    sty @(+ 2 vfile_handles)
    lda #<devcon_keyboard_vops
    sta vfile_ops_l
    lda #>devcon_keyboard_vops
    sta vfile_ops_h
    lda #<devcon_screen_vops
    sta @(++ vfile_ops_l)
    sta @(+ 2 vfile_ops_l)
    lda #>devcon_screen_vops
    sta @(++ vfile_ops_h)
    sta @(+ 2 vfile_ops_h)

    rts

devcon_clear_screen:
    lda #0
    sta devcon_cursor_x
    sta devcon_cursor_y
    jsr clear_screen
    jmp devcon_draw_cursor

devcon_print_charset:
    lda #0
    sta xpos
    lda #2
    sta ypos

    lda #32
    jsr devcon_print
    ldx #0
l:  txa
    jsr devcon_print
    and #15
    cmp #15
    bne +n
    lda #0
    sta xpos
    lda ypos
    clc
    adc #12
    sta ypos
n:  lda xpos
    clc
    adc #4
    sta xpos
    inx
    bne -l
    rts

devcon_read_keyboard:
    jsr take_over
    jsr wait_key
    clc
    jmp release

; X: vfile index
; A: character
devcon_write_screen:
    jsr take_over
    jsr devcon_print_ctrl
    clc
    jmp release
