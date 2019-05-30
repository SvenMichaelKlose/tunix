.exportzp s, d, c, tmp, base, ptr, size
.exportzp tmp5, tmp6, scr, tmp7, tmp8, do_load_library, do_make_jumps_to_core, last_error, zp_end_core

.zeropage

s:      .res 4     ; Source pointer.
d:      .res 4     ; Destination pointer.
c:      .res 4     ; Counter.

base:   .res 4
ptr:    .res 4
size:   .res 4

; Temporaries.
tmp:    .byte 0
;tmp2:   .byte 0
;tmp3:   .byte 0
;tmp4:   .byte 0
tmp5:   .byte 0
tmp6:   .byte 0
scr:
tmp7:   .byte 0
tmp8:   .byte 0

do_load_library:       .byte 0
do_make_jumps_to_core: .byte 0

last_error: .byte 0

zp_end_core:
