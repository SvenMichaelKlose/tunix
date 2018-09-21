.exportzp s, d, c, tmp
.exportzp tmp5, tmp6, scr, tmp7, tmp8, do_load_library, do_make_jumps_to_core, last_error, zp_end_core

.zeropage

s:      .word 0     ; Source pointer.
d:      .word 0     ; Destination pointer.
c:      .word 0     ; Counter.

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
