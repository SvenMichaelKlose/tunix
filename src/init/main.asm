init:
    lda #<txt_init
    ldy #>txt_init
    jsr $cb1e
    rts

txt_init:
    @(ascii2petscii "INIT PROCESS RUNNING.") 13 0
