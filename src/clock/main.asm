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

    rts

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
