; UltiFS wedge
;
; Wraps the UltiFS C functions by banking in missing banks.
; Also redirects the old vectors.

.export _blk2
.export _blk3

.export _init_secondary_wedge
.export uopen, uclose, uchkin, uckout, uclrcn, ubasin, ubsout, uclall, uload, usave

.export _last_regular_device

.import _ultifs_kopen
.import _ultifs_kclose
.import _ultifs_kchkin
.import _ultifs_kchkout
.import _ultifs_kclrcn
.import _ultifs_kbasin
.import _ultifs_kbsout
.import _ultifs_kclall
.import _ultifs_kusrcmd
.import _ultifs_kload
.import _ultifs_ksave
.import unmap_ofs

unmap = $9800 + unmap_ofs

.import __ZP_START__
.import __ZP_SIZE__

.segment "SECONDARY"

FA      = $ba   ; Device number

IOPEN   = $031A
ICLOSE  = $031C
ICHKIN  = $031E
ICKOUT  = $0320
ICLRCN  = $0322
IBASIN  = $0324
IBSOUT  = $0326
ISTOP   = $0328
IGETIN  = $032A
ICLALL  = $032C
IUSRCMD = $032E
ILOAD   = $0330
ISAVE   = $0332

_blk2:      .res 1
_blk3:      .res 1
old_blk2:   .res 1
old_blk3:   .res 1

_last_regular_device:   .res 1
_last_ingle_device:     .res 1

old_IOPEN:      .res 2
old_ICLOSE:     .res 2
old_ICHKIN:     .res 2
old_ICHKOUT:    .res 2
old_ICLRCN:     .res 2
old_IBASIN:     .res 2
old_IBSOUT:     .res 2
old_ISTOP:      .res 2
old_IGETIN:     .res 2
old_ICLALL:     .res 2
old_IUSRCMD:    .res 2  ; TODO: Make it point to a BRK.
old_ILOAD:      .res 2
old_ISAVE:      .res 2

_new_vectors:
    .word uopen
    .word uclose
    .word uchkin
    .word uckout
    .word uclrcn
    .word ubasin
    .word ubsout
    .word ustop
    .word ugetin
    .word uclall
    .word 0         ; TODO: Point to a BRK.
    .word uload
    .word usave
_new_vectors_end:

_saved_zp:  .res 256

.proc init_kernal_vectors
    ldx #_new_vectors_end - _new_vectors - 1
l:  lda IOPEN,x
    sta old_IOPEN,x
    lda _new_vectors,x
    sta IOPEN,x
    dex
    bpl l
    rts
.endproc

.proc _init_secondary_wedge
    tax
    stx _last_ingle_device
    dex
    stx _last_regular_device

;    jsr init_kernal_vectors
    rts
.endproc

; Returns CC if it's not.
.proc is_our_device
    pha
    lda FA
    cmp _last_ingle_device
    pla
    bcc done
    pha
    lda FA
    cmp _last_regular_device
    pla
done:
    rts
.endproc

.proc enter
    sta $100
    stx $101
    sty $102
    php
    pla
    sta $103

    lda $9ffa
    sta $107
    lda $9ffb
    sta $108
    lda $9ffc
    sta $109
    lda $9ffd
    sta $10a
    lda #118
    sta $9ffa
    lda #119
    sta $9ffc
    lda #0
    sta $9ffb
    sta $9ffd

    ; Save zeropage.
    ldx #<__ZP_SIZE__
l:  lda __ZP_START__,x
    sta _saved_zp,x
    dex
    bne l

    rts
.endproc

.proc leave
    ; Restore zeropage.
    ldx #<__ZP_SIZE__
l:  lda _saved_zp,x
    sta __ZP_START__,x
    dex
    bne l

    lda $107
    sta $9ffa
    lda $108
    sta $9ffb
    lda $109
    sta $9ffc
    lda $10a
    sta $9ffd

    ldx $101
    ldy $102
    jmp unmap ; Accum flags and BLK1 will be restored there.
.endproc

.proc uopen
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kopen
    jmp leave

n:  lda old_IOPEN
    pha
    lda old_IOPEN + 1
    pha
    jmp unmap
.endproc

.proc uclose
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclose
    jmp leave

n:  lda old_ICLOSE
    pha
    lda old_ICLOSE + 1
    pha
    jmp unmap
.endproc

.proc uchkin
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kchkin
    jmp leave

n:  lda old_ICHKIN
    pha
    lda old_ICHKIN + 1
    pha
    jmp unmap
.endproc

.proc uckout
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kchkout
    jmp leave

n:  lda old_ICHKOUT
    pha
    lda old_ICHKOUT + 1
    pha
    jmp unmap
.endproc

.proc uclrcn
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclrcn
    jmp leave

n:  lda old_ICLRCN
    pha
    lda old_ICLRCN + 1
    pha
    jmp unmap
.endproc

.proc ubasin
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kbasin
    jmp leave

n:  lda old_IBASIN
    pha
    lda old_IBASIN + 1
    pha
    jmp unmap
.endproc

.proc ubsout
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kbsout
    jmp leave

n:  lda old_IBSOUT
    pha
    lda old_IBSOUT + 1
    pha
    jmp unmap
.endproc

.proc ustop
n:  lda old_ISTOP
    pha
    lda old_ISTOP + 1
    pha
    jmp unmap
.endproc

.proc ugetin
n:  lda old_IGETIN
    pha
    lda old_IGETIN + 1
    pha
    jmp unmap
.endproc

.proc uclall
    jsr enter
    jsr _ultifs_kclall
    lda old_ICLALL
    pha
    lda old_ICLALL + 1
    pha
    jmp leave
.endproc

.proc uload
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kload
    jmp leave

n:  lda old_ILOAD
    pha
    lda old_ILOAD + 1
    pha
    jmp unmap
.endproc

.proc usave
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_ksave
    jmp leave

n:  lda old_ISAVE
    pha
    lda old_ISAVE + 1
    pha
    jmp unmap
.endproc
