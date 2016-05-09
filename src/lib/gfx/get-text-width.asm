.export get_text_width

.importzp xpos, ypos, pencil_mode, s
.import putstring

.code

; s: Zero–terminated string
;
; A: Width in pixels.
.proc get_text_width
    lda xpos
    pha
    lda ypos
    pha
    lda pencil_mode
    pha

    lda #0
    sta pencil_mode
    sta xpos
    sta ypos
    jsr putstring
    ldy xpos

    pla
    sta pencil_mode
    pla
    sta ypos
    pla
    sta xpos

    tya
    rts
.endproc
