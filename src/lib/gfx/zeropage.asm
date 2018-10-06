.exportzp p, ph, bca, bcp, srx, xcpos, xcpos2, font_compression
.exportzp rxl, ryt, rxr, ryb, region_sp, xpos, ypos, xpos2, ypos2, width, height, pattern
.exportzp font, font_bank, font_space_size, do_compress_font_gaps, pencil_mode, scrbase
.exportzp masks, maskd
.exportzp context_start, context_end, context_size

.zeropage

p:      .res 1
ph:     .res 1

bca:    .res 2     ; Bytecode argument list.
bcp:    .res 2     ; Bytecode pointer.
srx:    .res 1     ; Saved X register.

xcpos:  .res 1     ; X columns.
xcpos2: .res 1     ; X columns.


; Utils

font_compression: .res 1

;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing context ;;;
;;;;;;;;;;;;;;;;;;;;;;;

context_start:

; Visible region.
rxl:    .res 1
ryt:    .res 1
rxr:    .res 1
ryb:    .res 1

; Region stack.
region_sp:
        .res 2

; Cursor
xpos:   .res 1     ; X position
ypos:   .res 1     ; Y position
xpos2:  .res 1     ; X position
ypos2:  .res 1     ; Y position
width:  .res 1     ; Width
height: .res 1     ; Height

; XXX Pencil mode isn't regarded in all places.
pencil_mode:
        .res 1     ; 0: Don't draw.
                   ; 1: OR pixel values.
                   ; 2: XOR pixel values.
pattern:.res 2     ; Address of 8 byte pattern.

font:   .res 2
font_bank: .res 1
font_space_size: .res 1 ; Width of an empty character.
do_compress_font_gaps: .res 1

masks:  .res 1     ; Source mask.
maskd:  .res 1     ; Destination mask.

scrbase:.res 2     ; Screen bitmap base address.

context_end:

context_size = context_end-context_start
