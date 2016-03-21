    lda #<syms_core
    sta s
    lda #>syms_core
    sta @(++ s)
    lda #<jt_core
    sta d
    lda #>jt_core
    sta @(++ d)
    jsr $0400

    ldx #<txt_prompt
    ldy #>txt_prompt
    jsr print

l:  inc $1e01
    jmp -l

txt_prompt:
    @(ascii2petscii ">") 0

syms_core:
    "g" 0
    "inc_s" 0
    "inc_d" 0
    "launch" 0
    0

jt_core:
inc_s: 0 0 0
inc_d: 0 0 0
launch: 0 0 0
