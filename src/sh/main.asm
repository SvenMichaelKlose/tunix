program_size = @(- program_end program_start)

    $00 $20
    <program_size >program_size
    org $2000

program_start:
    ldx #<txt_prompt
    ldy #>txt_prompt
    jsr $cb1e
    rts

txt_prompt:
    ">" 0

program_end:
