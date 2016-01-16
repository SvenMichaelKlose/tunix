; SYSCALL: Calculate screen bitmap address
;
; xpos: X position (pixels)
; ypos: Y position
;
; Returns:
; scr:    Address of character line
; xcpos:  X position (column)
calc_scr:
    lda xpos
    lsr
    lsr
    lsr
    sta xcpos
    tax
    lda ypos
    clc
    adc column_addrs_lo,x
    sta scr
    lda column_addrs_hi,x
    clc
    adc #0
    sta @(++ scr)
    rts

column_addrs_lo: @(maptimes [low (+ charset (* 16 screen_rows _))] screen_columns)
column_addrs_hi: @(maptimes [high (+ charset (* 16 screen_rows _))] screen_columns)

vmaskd: %01111111
        %10111111
        %11011111
        %11101111
        %11110111
        %11111011
        %11111101
        %11111110

vmasks: %10000000
        %01000000
        %00100000
        %00010000
        %00001000
        %00000100
        %00000010
        %00000001

; SYSCALL: Draw a vertical line
;
; xpos:     top X position
; ypos:     top Y position
; height:   -
; pattern:  8-byte pattern to draw
vline:
    lda xpos
    and #7
    tax
    lda vmasks,x
    sta masks
    lda vmaskd,x
    sta maskd

; SYSCALL: Fill part of column
;
; In:   xpos. ypos, height, pattern
;       masks:  Source mask (ANDed with pattern).
;       maskd:  Destination mask (ANDed with screen).
vfill:
    jsr calc_scr

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
