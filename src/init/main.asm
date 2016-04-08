init:
    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    lda #1          ; Wait until process finishes.
    jsr launch

    jsr guru_meditation

path_sh:
    @(ascii2petscii "SH") 0
