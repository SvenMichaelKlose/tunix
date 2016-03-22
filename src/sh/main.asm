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

    ; Get jumps to gfx library.
    lda #<syms_gfx
    sta s
    lda #>syms_gfx
    sta @(++ s)
    lda #<jt_gfx
    sta d
    lda #>jt_gfx
    sta @(++ d)
    jsr $0400

    ; Print text.
    ldx #<txt_prompt
    ldy #>txt_prompt
    jsr print

;    jsr boot

    ; Show that we're multitasking.
l:  inc $1e01
    jmp -l

txt_prompt:
    @(ascii2petscii "HELLO WORLD!") 0

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

syms_gfx:
    "GFX" 0
    "calcscr" 0
    "boot" 0
    0

jt_gfx:
calcscr:    0 0 0
boot:       0 0 0
