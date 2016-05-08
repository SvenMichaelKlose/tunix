.export reset_region

.importzp rxl, c_setzs, screen_width, screen_height

.code

.proc reset_region
    brk
    .byte c_setzs, rxl, 4, 0, 0, screen_width-1, screen_height-1
    .byte 0
    rts
.endproc
