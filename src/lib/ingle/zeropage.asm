.exportzp s, d, c, tmp, base, ptr, size
.exportzp tmp5, tmp6, scr, tmp7, tmp8

.zeropage

s = 0 ; Source pointer.
d = 4 ; Destination pointer.
c = 8 ; Counter.

base:   .res 4
ptr:    .res 4
size:   .res 4

; Temporaries.
tmp:    .res 1
tmp5:   .res 1
tmp6:   .res 1
scr:
tmp7:   .res 1
tmp8:   .res 1

zp_end_core:
