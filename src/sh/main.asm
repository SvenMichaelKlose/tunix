    ldx #<txt_prompt
    ldy #>txt_prompt
    jsr print

l:  inc $1e01
    jmp -l

txt_prompt:
    @(ascii2petscii ">") 0
