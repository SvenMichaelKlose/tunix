.export vfill, vcopy

.importzp scr, pattern, ypos, height, masks, maskd, tmp

.code

; SYSCALL: Fill part of column
;
; In:   ypos, height, pattern
;       masks:  Source mask (ANDed with pattern).
;       maskd:  Destination mask (ANDed with screen).
;       scr:    Starting address.
.proc vfill
    lda pattern
    sta mod_pattern+1
    lda pattern+1
    sta mod_pattern+2
    lda ypos
    and #7
    tax
    ldy height
l:  lda (scr),y
    and maskd
    sta tmp
mod_pattern:
    lda $ffff,x
    and masks
    ora tmp
    sta (scr),y
    inx
    txa
    and #7
    tax
    dey
    bne l

    rts
.endproc

; In:   ypos, height, pattern
;       scr:    Starting address.
.proc vcopy
    lda pattern
    sta mod_pattern+1
    lda pattern+1
    sta mod_pattern+2
    lda ypos
    and #7
    tax
    ldy height
l:  
mod_pattern:
    lda $ffff,x
    sta (scr),y
    inx
    txa
    and #7
    tax
    dey
    bne l

    rts
.endproc
