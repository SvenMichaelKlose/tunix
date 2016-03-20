init:
    ; Reset "overtake".
    lda #0
    sta takeovers

    ; Disable interrupts and NMI.
    lda #$7F
    sta $911d
    sta $912e

    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    jsr launch

init_daemon:

l:  inc $1e00
    jmp -l

txt_init:
    @(ascii2petscii "INIT PROCESS RUNNING.") 13 0

path_sh:
    @(ascii2petscii "SH") 0
