; SYSCALL: Fill part of column
;
; In:   xpos. ypos, height, pattern
;       masks:  Source mask (ANDed with pattern).
;       maskd:  Destination mask (ANDed with screen).
vfill:
    jsr calcscr

    lda pattern
    sta @(+ 1 +mod_pattern)
    lda @(+ 1 pattern)
    sta @(+ 2 +mod_pattern)
    ldy height
    dey
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
    cpy #255
    bne -l

    rts
