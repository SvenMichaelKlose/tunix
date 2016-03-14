    "calcscr" 0 <calcscr >calcscr
    "setpattern" 0 <setpattern >setpattern
    "vline" 0 <vline >vline
    "hline" 0 <hline >hline
    "frame" 0 <frame >frame
    "box" 0 <box >box
    "putchar" 0 <putchar >putchar
    "putstring" 0 <putstring >putstring
    0

    org $2000

    jmp gfx_init

    ; Reset VIC to KERNAL defaults.
    ldx #15
l:  lda $ede4,x
    sta $9000,x
    dex
    bpl -l
    rts
