.export reset_region

.importzp rxl
.importzp c_setzs

.include "vic-settings.inc.asm"

.code

.proc reset_region
    brk
    .byte c_setzs, rxl, 4, 0, 0, screen_width-1, screen_height-1
    .byte 0
    rts
.endproc
