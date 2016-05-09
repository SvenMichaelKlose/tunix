.exportzp p, ph, bca, bcp, srx, xcpos, font_compression
.exportzp rxl, ryt, rxr, ryb, xpos, ypos, xpos2, ypos2, width, height, pattern, patternh
.exportzp font, font_space_size, do_compress_font_gaps
.exportzp masks, maskd
.exportzp context_start, context_end, context_size

.zeropage

p:      .byte 0
ph:     .byte 0

bca:    .word 0     ; Bytecode argument list.
bcp:    .word 0     ; Bytecode pointer.
srx:    .byte 0       ; Saved X register.

xcpos:  .byte 0       ; X columns.


; Utils

font_compression: .byte 0

;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing context ;;;
;;;;;;;;;;;;;;;;;;;;;;;

context_start:

; Visible region.
rxl:    .byte 0
ryt:    .byte 0
rxr:    .byte 0
ryb:    .byte 0

; Cursor
xpos:   .byte 0       ; X position
ypos:   .byte 0       ; Y position
xpos2:  .byte 0       ; X position
ypos2:  .byte 0       ; Y position
width:  .byte 0       ; Width
height: .byte 0       ; Height

pattern: .byte 0        ; Pencil for everything.
patternh: .byte 0

font:   .word 0
font_space_size: .byte 0 ; Width of an empty character.
do_compress_font_gaps: .byte 0

masks:  .byte 0       ; Source mask.
maskd:  .byte 0       ; Destination mask.

context_end:

context_size = context_end-context_start
