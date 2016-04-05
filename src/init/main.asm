init:
    jsr wait_key
    jsr devcon_print
    jmp init

    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    lda #1      ; Wait until launched process has been killed.
    jsr launch

    ; Show that we're multitasking.
l:  inc $1e00
    jmp -l

path_sh:
    @(ascii2petscii "SH") 0
