boot:
    lda #<exec_script
    sta $316
    lda #>exec_script
    sta $317

    brk
    c_setzw s $00 $88
    c_setzw d $00 $30
    c_setzb font_compression 2
    0
    jsr compress_font

    jsr gfx_init

    brk
    c_setzb xpos 0
    c_setzb ypos 0
    c_setzb width 80
    c_setzb height 191
    0
    jsr window

    brk
    c_setzb xpos 80
    c_setzb ypos 0
    c_setzb width 79
    c_setzb height 191
    0
    jsr window

l:  jsr $ffe4
    beq -l

    ldx #15
    lda #0
l:  sta $9000,x
    dex
    bpl -l

    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    ldx #@(- trampoline_end trampoline)
l:  lda trampoline,x
    sta $1d00,x
    dex
    bpl -l
    jmp $1d00

trampoline:
    lda #%00000000
    sta $9ff2

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init VIC.

    cli
    jmp ($c000)
trampoline_end:
