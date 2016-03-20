init:
    jsr init_task_switching

    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    jsr launch
l:  jmp -l

txt_init:
    @(ascii2petscii "INIT PROCESS RUNNING.") 13 0

path_sh:
    @(ascii2petscii "SH") 0
