.export memmove

.importzp s, d, c
.import zpw_add_xy
.import zpw_cmp_xy
.import zpw_dec_x
.import moveram

; Auto-detects if copy should be
; forwards or backwards.  It's backwards
; if the destination 'd' is before the
; source 's' address.
.export memmove
.proc memmove
    ldx #s
    ldy #d
    jsr zpw_cmp_xy
    bcc :+
    lda #0
    beq :++ ; (jmp)
:   lda c
    pha
    lda c+1
    pha
    ldx #c
    jsr zpw_dec_x
    ldx #s
    ldy #c
    jsr zpw_add_xy
    ldx #d
    jsr zpw_add_xy
    pla
    sta c+1
    pla
    sta c
    lda #1
:   jmp moveram
.endproc
