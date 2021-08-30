.export reset_region

.importzp rxl, ryt, rxr, ryb, c_setzs, screen_width, screen_height

.code

.proc reset_region
    lda #0
    sta rxl
    sta ryt
    lda #screen_width
    sta rxr
    lda #screen_height
    sta ryb
    rts
.endproc
