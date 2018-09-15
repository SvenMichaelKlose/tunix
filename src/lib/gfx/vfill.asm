.export vfill, vcopy

.importzp scr, pattern, ypos, height, masks, maskd, tmp
.importzp pencil_mode

.code

; SYSCALL: Fill part of column
;
; In:   ypos, height, pattern
;       masks:  Source mask (ANDed with pattern).
;       maskd:  Destination mask (ANDed with screen).
;       scr:    Starting address.
.proc vfill
    lda ypos
    and #7
    tax
    ldy height
    lda pencil_mode
    beq r
    cmp #1
    beq pm_write

pm_xor:
    lda pattern
    sta mod_pattern+1
    lda pattern+1
    sta mod_pattern+2
l:  lda (scr),y
    and maskd
    sta tmp
mod_pattern:
    lda $ffff,x
    and masks
    ora tmp
    eor (scr),y
    sta (scr),y
    inx
    txa
    and #7
    tax
    dey
    bne l
r:  rts

pm_write:
    lda pattern
    sta mod_pattern2+1
    lda pattern+1
    sta mod_pattern2+2
l2: lda (scr),y
    and maskd
    sta tmp
mod_pattern2:
    lda $ffff,x
    and masks
    ora tmp
    sta (scr),y
    inx
    txa
    and #7
    tax
    dey
    bne l2

    rts
.endproc

; In:   ypos, height, pattern
;       scr:    Starting address.
.proc vcopy
    lda ypos
    and #7
    tax
    ldy height
    lda pencil_mode
    beq r
    cmp #1
    beq pm_write

pm_xor:
    lda pattern
    sta mod_pattern+1
    lda pattern+1
    sta mod_pattern+2
l:  
mod_pattern:
    lda $ffff,x
    eor (scr),y
    sta (scr),y
    inx
    txa
    and #7
    tax
    dey
    bne l
    rts

pm_write:
    lda pattern
    sta mod_pattern2+1
    lda pattern+1
    sta mod_pattern2+2
l2:  
mod_pattern2:
    lda $ffff,x
    sta (scr),y
    inx
    txa
    and #7
    tax
    dey
    bne l2

r:  rts
.endproc
