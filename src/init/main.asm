init:
    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    lda #0
    jsr launch

    ; Show that we're multitasking.
l:  inc $1e00
    jmp -l

path_sh:
    @(ascii2petscii "SH") 0
