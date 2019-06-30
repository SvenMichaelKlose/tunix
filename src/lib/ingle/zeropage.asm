.exportzp s, d, c, tmp, base, ptr, size
.exportzp tmp5, tmp6, scr, tmp7, tmp8
;.exportzp do_load_library, do_make_jumps_to_core, last_error, zp_end_core

.zeropage

s:      .res 4 ; Source pointer.
d:      .res 4 ; Destination pointer.
c:      .res 4 ; Counter.

base:   .res 4
ptr:    .res 4
size:   .res 4

; Temporaries.
tmp:    .res 1
;tmp2:   .res 1
;tmp3:   .res 1
;tmp4:   .res 1
tmp5:   .res 1
tmp6:   .res 1
scr:
tmp7:   .res 1
tmp8:   .res 1

;do_load_library:       .res 1
;do_make_jumps_to_core: .res 1

;last_error: .res 1

zp_end_core:
