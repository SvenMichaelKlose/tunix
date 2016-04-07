init:
    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    lda #0
    jsr launch
    bcs +g

    ; Show that we're multitasking.
l:  inc $1e00
    jmp -l

g:  jsr guru_meditation

path_sh:
    @(ascii2petscii "SH") 0
