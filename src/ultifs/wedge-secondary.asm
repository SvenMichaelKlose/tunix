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
.import _ultifs_kload
.import _ultifs_ksave

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
old_IBASOUT:    .res 2
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

_saved_zp:  .res $80

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

.proc copy_zp
    ldx #$20
l:  lda __ZP_START__,x
    sta _saved_zp,x
    dex
    bpl l
    rts
.endproc

.proc _init_secondary_wedge
    tax
    stx _last_ingle_device
    dex
    stx _last_regular_device

    jsr init_kernal_vectors
    rts ;jmp copy_zp
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
rts
    php
    pha
    txa
    pha
    tya
    pha

;    lda $9ffa
;    sta old_blk2
;    lda $9ffc
;    sta old_blk3
;    lda _blk2
;    sta $9ffa
;    lda _blk3
;    sta $9ffc
    jsr swap_zp

    pla
    tay
    pla
    tax
    pla
    plp

    rts
.endproc

.proc leave
;    lda old_blk2
;    sta $9ffa
;    lda old_blk3
;    sta $9ffc
;    jsr swap_zp

    lda $103
    pha
    lda $100
    ldx $101
    ldy $102
    plp

    rts
.endproc

.proc uopen
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kopen
    jmp leave
n:  jmp (old_IOPEN)
.endproc

.proc uclose
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclose
    jmp leave
n:  jmp (old_ICLOSE)
.endproc

.proc uchkin
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kchkin
    jmp leave
n:  jmp (old_ICHKIN)
.endproc

.proc uckout
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kchkout
    jmp leave
n:  jmp (old_ICHKOUT)
.endproc

.proc uclrcn
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclrcn
    jmp leave
n:  jmp (old_ICLRCN)
.endproc

.proc ubasin
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kbasin
    jmp leave
n:  jmp (old_IBASIN)
.endproc

.proc ubsout
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kbsout
    jmp leave
n:  jmp (old_IBASOUT)
.endproc

.proc ustop
    jmp (old_ISTOP)
.endproc

.proc ugetin
    jmp (old_IGETIN)
.endproc

.proc uclall
    jsr enter
    jsr _ultifs_kclall
    jsr leave
    jmp (old_ICLALL)
.endproc

.proc uload
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kload
    jmp leave
n:  jmp (old_ICLALL)
.endproc

.proc usave
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_ksave
    jmp leave
n:  jmp (old_ICLALL)
.endproc
