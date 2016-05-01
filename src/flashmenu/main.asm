boot:
if @(not *no-core?*)
    ; Get jumps to core.
    lda #<syms_core
    sta s
    lda #>syms_core
    sta @(++ s)
    lda #<jt_core
    sta d
    lda #>jt_core
    sta @(++ d)
    jsr $0400
end

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

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init VIC.

    cli
    jmp ($c000)

txt_welcome:
    "Commodore VIC-20 GUI" 0
    " " 0
    " " 0
    "Graphics primitives and" 0
    "variable width font rendering" 0
    "in less than 2K." 0
    " " 0
    "This font is generated from" 0
    "the system font at startup." 0
    " " 0
    "All drawing is clipped." 0
    0

;;; Wanted jumps to the core.
syms_core:
    "/g" 0
    "inc_s" 0
    "take_over" 0
    "release" 0
    0

;;; Jump table to core.

jt_core:
inc_s:      0 0 0
take_over:  0 0 0
release:    0 0 0
