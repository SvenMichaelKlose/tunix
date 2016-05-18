.export vfill, vcopy

.import calcscr
.importzp scr, pattern, height, masks, maskd, tmp

.code

; SYSCALL: Fill part of column
;
; In:   height, pattern
;       masks:  Source mask (ANDed with pattern).
;       maskd:  Destination mask (ANDed with screen).
;       scr:    Starting address.
.proc vfill
    lda pattern
    sta mod_pattern+1
    lda pattern+1
    sta mod_pattern+2
    ldy height
l:  tya
    and #7
    tax
    lda (scr),y
    and maskd
    sta tmp
mod_pattern:
    lda $ffff,x
    and masks
    ora tmp
    sta (scr),y
    dey
    bne l

    rts
.endproc

.proc vcopy
    lda pattern
    sta mod_pattern+1
    lda pattern+1
    sta mod_pattern+2
    ldy height
l:  tya
    and #7
    tax
mod_pattern:
    lda $ffff,x
    sta (scr),y
    dey
    bne l

    rts
.endproc


