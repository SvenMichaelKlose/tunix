; UltiFS-space wedge
;
; This wraps to the UltiFS C functions.
; If another device is being accessed, the old function
; vector is being called.
;
; Most likely this will map in the rest of the UltiFS
; code the primary wedge did not care about to keep it
; small.
; Also the zero page has to be set up before C functions
; are being called.

.export _blk2

.import _ultifs_open
.import __ZP_START__
.import __ZP_SIZE__

.segment "SECONDARY"

_blk2:      .res 1

_saved_zp:  .res $80

.proc swap_zp
    ldx #$20
l:  lda __ZP_START__,x
    pha
    lda _saved_zp,x
    sta __ZP_START__,x
    pla
    sta _saved_zp,x
    dex
    bpl l
    rts
.endproc

.proc uopen
    lda _blk2
    sta $9ffa
    jsr swap_zp
    jsr _ultifs_open
    jmp swap_zp
.endproc
