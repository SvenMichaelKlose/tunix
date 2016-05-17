.export calcscr

.importzp xpos, xcpos, ypos, scr, scrbase
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
    tay
    lda column_addrs_hi,x
    adc #0
    sta scr+1
    tya
    clc
    adc scrbase
    sta scr
    lda scr+1
    adc scrbase+1
    sta scr+1
    rts
.endproc
