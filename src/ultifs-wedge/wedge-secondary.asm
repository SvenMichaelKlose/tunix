; UltiFS-space wedge
;
; Residing in BLK1 this wraps to the UltiFS C functions.
; If another device is being accessed, the old function
; vector is being called.
;
; Most likely this will map in the rest of the UltiFS
; code the primary wedge did not care about.

.segment "ULTIFS"

.proc uopen
    jmp _ultifs_open
.endproc
