init:
    lda #<txt_mounting_root
    sta s
    lda #>txt_mounting_root
    sta @(++ s)
    jsr devcon_print_string

    jsr devcbm_make_root

    lda #<txt_starting_sh
    sta s
    lda #>txt_starting_sh
    sta @(++ s)
    jsr devcon_print_string

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

txt_mounting_root:
    "Mounting root file system..." 10 0

txt_shutdown:
    "g shut down. Have a nice day!" 0

txt_starting_sh:
    "Starting sh..." 10 10 0

path_sh:
    @(ascii2petscii "SH") 0
