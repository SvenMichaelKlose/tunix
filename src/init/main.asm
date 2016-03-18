init:
    lda #<txt_init
    ldy #>txt_init
    jsr $cb1e

    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    jsr load
    rts

txt_init:
    @(ascii2petscii "INIT PROCESS RUNNING.") 13 0

path_sh:
    @(ascii2petscii "SH") 0
