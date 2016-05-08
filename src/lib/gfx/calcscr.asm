.export calcscr

.importzp xpos, xcpos, ypos, scr
.import column_addrs_lo, column_addrs_hi

.code

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
