    ldx #<txt_prompt
    ldy #>txt_prompt
    jsr print
stop:
    rts

txt_prompt:
    @(ascii2petscii ">") 0
