    ldx #<txt_prompt
    ldy #>txt_prompt
stop:
    jsr print
    rts

txt_prompt:
    @(ascii2petscii ">") 0
