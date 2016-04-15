init:
    jsr devcbm_make_root

    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    lda #1          ; Wait until process finishes.
    jsr launch

    lda #<txt_shutdown
    sta s
    lda #>txt_shutdown
    sta @(++ s)
    jsr devcon_print_string

w:  jmp -w

txt_shutdown:
    "g shut down. Have a nice day!" 0

path_sh:
    @(ascii2petscii "SH") 0
