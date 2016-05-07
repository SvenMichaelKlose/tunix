.export calcscr
.import xpos, xcpos, ypos, column_addrs_lo, column_addrs_hi, scr
.segment "CODE"

; SYSCALL: Calculate screen bitmap address
;
; xpos: X position (pixels)
; ypos: Y position
;
; Returns:
; scr:    Address of character line
; xcpos:  X position (column)
.proc calcscr
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
    adc #0
    sta scr+1
    rts
.endproc

column_addrs_lo: @(maptimes [low (+ charset (* 16 screen_rows _))] screen_columns)
column_addrs_hi: @(maptimes [high (+ charset (* 16 screen_rows _))] screen_columns)
