; UltiFS-space wedge
;
; This wraps to the UltiFS C functions.
; If another device is being accessed, the old function
; vector is being called.
;
; Most likely this will map in the rest of the UltiFS
; code the primary wedge did not care about.
; Also the zero page has to be set up before C functions
; are called.

.import _ultifs_open

.segment "ULTIFS"

.proc uopen
    jmp _ultifs_open
.endproc
