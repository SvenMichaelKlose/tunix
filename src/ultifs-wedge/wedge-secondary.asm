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

.export init_secondary_wedge

.import _ultifs_kopen
.import _ultifs_kclose
.import _ultifs_kchkin
.import _ultifs_kchkout
.import _ultifs_kclrcn
.import _ultifs_kbasin
.import _ultifs_kbasout
.import _ultifs_kclall

.import __ZP_START__
.import __ZP_SIZE__

.segment "SECONDARY"

FA      = $ba   ; Device number

IOPEN   = $031A     ; KERNAL vector - open a logical file
ICLOSE  = $031C     ; KERNAL vector - close a specified logical file
ICHKIN  = $031E     ; KERNAL vector - open channel for input
ICKOUT  = $0320     ; KERNAL vector - open channel for output
ICLRCN  = $0322     ; KERNAL vector - close input and output channels
IBASIN  = $0324     ; KERNAL vector - input character from channel
IBSOUT  = $0326     ; KERNAL vector - output character to channel
ICLALL  = $032C     ; KERNAL vector - close all channels and files
ILOAD   = $0330     ; KERNAL vector - load
ISAVE   = $0332     ; KERNAL vector - save

_blk2:      .res 1
old_blk2:   .res 1
_udevs:     .res 1

old_IOPEN:      .res 2
old_ICLOSE:     .res 2
old_ICHKIN:     .res 2
old_ICHKOUT:    .res 2
old_ICLRCN:     .res 2
old_IBASIN:     .res 2
old_IBASOUT:    .res 2
old_ICLALL:     .res 2

_new_vectors:
new_IOPEN:      .word uopen
new_ICLOSE:     .word uclose
new_ICHKIN:     .word uchkin
new_ICHKOUT:    .word uchkout
new_ICLRCN:     .word uclrcn
new_IBASIN:     .word ubasin
new_IBASOUT:    .word ubasout
new_ICLALL:     .word uclall

_saved_zp:  .res $80

.proc init_kernal_vectors
    ldx #15
l:  lda IOPEN,x
    sta old_IOPEN,x
    lda _new_vectors,x
    sta IOPEN,x
    dex
    bpl l
    rts
.endproc

.proc init_secondary_wedge
    jsr init_kernal_vectors
    lda #11
    sta _udevs
    rts
.endproc

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

.proc is_our_device
    pha
    lda FA
    cmp _udevs
    pla
    rts
.endproc

.proc enter
    lda _blk2
    sta $9ffa
    jmp swap_zp
.endproc

.proc uopen
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kopen
    jmp swap_zp
n:  jmp (old_IOPEN)
.endproc

.proc uclose
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclose
    jmp swap_zp
n:  jmp (old_ICLOSE)
.endproc

.proc uchkin
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kchkin
    jmp swap_zp
n:  jmp (old_ICHKIN)
.endproc

.proc uchkout
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kchkin
    jmp swap_zp
n:  jmp (old_ICHKOUT)
.endproc

.proc uclrcn
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclrcn
    jmp swap_zp
n:  jmp (old_ICLRCN)
.endproc

.proc ubasin
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kbasin
    jmp swap_zp
n:  jmp (old_IBASIN)
.endproc

.proc ubasout
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kbasout
    jmp swap_zp
n:  jmp (old_IBASOUT)
.endproc

.proc uclall
    jsr enter
    jsr _ultifs_kclall
    jsr swap_zp
    jmp (old_ICLALL)
.endproc
