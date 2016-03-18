    ;;; Empty list of symbols.
    $01 $00     ; Its size.
    0

    $00 $20
    org $2000

    ldx #<txt_prompt
    ldy #>txt_prompt
    jsr $cb1e
    rts

txt_prompt:
    ">" 0
