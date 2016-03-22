    jmp gfx_init

    ; Reset VIC to KERNAL defaults.
    ldx #15
l:  lda $ede4,x
    sta $9000,x
    dex
    bpl -l
    rts
