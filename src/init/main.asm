loaded_init:
    org $2000

init:
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

txt_init:
    @(ascii2petscii "INIT PROCESS RUNNING.") 13 0

path_sh:
    @(ascii2petscii "SH") 0

init_end:
    org @(+ loaded_init (- init_end init))
