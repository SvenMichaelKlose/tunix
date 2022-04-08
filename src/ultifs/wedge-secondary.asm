; UltiFS wedge
;
; Wraps the UltiFS C functions by banking BLK2 and BLK3.
; Also redirects the old vectors.

.export _init_secondary_wedge
.export _last_regular_device
.export _saved_zp
.export uopen, uclose, uchkin, uckout, uclrcn
.export ubasin, ubsout, uclall, uload, usave

.import _ultifs_kopen, _ultifs_kclose, _ultifs_kchkin, _ultifs_kchkout
.import _ultifs_kclrcn, _ultifs_kbasin, _ultifs_kbsout, _ultifs_kclall
.import _ultifs_kusrcmd, _ultifs_kload, _ultifs_ksave
.import unmap_ofs

stack_size  = $2a    ; Keep greater or equal to what linker config file says.
cpu_state   = $9c00
unmap       = $9800 + unmap_ofs

.import __ZP_START__
.import __ZP_SIZE__

.segment "SECONDARY"

FA      = $ba   ; Device number
DFLTN   = $99
DFLTO   = $9a

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

_last_regular_device:   .res 1
_last_ingle_device:     .res 1

old_IOPEN:      .res 2
old_ICLOSE:     .res 2
old_ICHKIN:     .res 2
old_ICHKOUT:    .res 2
old_ICLRCN:     .res 2
old_IBASIN:     .res 2
old_IBSOUT:     .res 2
old_ISTOP:      .res 2  ; unchanged
old_IGETIN:     .res 2  ; unchanged
old_ICLALL:     .res 2
old_IUSRCMD:    .res 2  ; unchanged
old_ILOAD:      .res 2
old_ISAVE:      .res 2

_saved_zp:  .res stack_size

.proc _init_secondary_wedge
    tax
    stx _last_ingle_device
    dex
    stx _last_regular_device

    ; Save KERNAL vectors with 1 off so they can serve as
    ; return addresses on the stack.
    ldx #0
    ldy #13
l:  lda IOPEN,x
    sec
    sbc #1
    sta old_IOPEN,x
    lda IOPEN+1,x
    sbc #0
    inx
    sta old_IOPEN,x
    inx
    dey
    bne l

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

; Returns CC if it's not.
.proc is_our_input
    pha
    lda DFLTN
    cmp _last_ingle_device
    pla
    bcc done

    pha
    lda DFLTN
    cmp _last_regular_device
    pla

done:
    rts
.endproc

; Returns CC if it's not.
.proc is_our_output
    pha
    lda DFLTO
    cmp _last_ingle_device
    pla
    bcc done

    pha
    lda DFLTO
    cmp _last_regular_device
    pla

done:
    rts
.endproc

.proc enter
    ; Save X and Y registers.  Accu and flags have
    ; already been saved by the primary wedge.
    stx cpu_state+1
    sty cpu_state+2

    ; Save banks.
    lda $9ffa
    sta cpu_state+7
    lda $9ffb
    sta cpu_state+8
    lda $9ffc
    sta cpu_state+9
    lda $9ffd
    sta cpu_state+10
    lda $9ffe
    sta cpu_state+11
    lda $9fff
    sta cpu_state+12

    ; Bank in rest of UltiFS at BLK2 and BLK3.
    lda #118
    sta $9ffa
    lda #119
    sta $9ffc
    lda #0
    sta $9ffb
    sta $9ffd

    ; Save zeropage.
    ldx #<__ZP_SIZE__
l:  lda __ZP_START__-1,x
    sta _saved_zp-1,x
    dex
    bne l

    rts
.endproc

.proc leave
    ; Restore zeropage.
    ldx #<__ZP_SIZE__
l:  lda _saved_zp-1,x
    sta __ZP_START__-1,x
    dex
    bne l

    ; Restore banks.
    lda cpu_state+7
    sta $9ffa
    lda cpu_state+8
    sta $9ffb
    lda cpu_state+9
    sta $9ffc
    lda cpu_state+10
    sta $9ffd
    lda cpu_state+11
    sta $9ffe
    lda cpu_state+12
    sta $9fff

    ; Restore X and Y register.  Accu, flags and BLK1
    ; will be restored by unmap().
    ldx cpu_state+1
    ldy cpu_state+2
    jmp unmap
.endproc

.proc uopen
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kopen
    jmp leave

n:  lda old_IOPEN+1
    pha
    lda old_IOPEN
    pha
    jmp unmap
.endproc

.proc uclose
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kclose
    jmp leave

n:  lda old_ICLOSE+1
    pha
    lda old_ICLOSE
    pha
    jmp unmap
.endproc

.proc uchkin
    jsr is_our_input
    bcc n
    jsr enter
    jsr _ultifs_kchkin
    jmp leave

n:  lda old_ICHKIN+1
    pha
    lda old_ICHKIN
    pha
    jmp unmap
.endproc

.proc uckout
    jsr is_our_output
    bcc n
    jsr enter
    jsr _ultifs_kchkout
    jmp leave

n:  lda old_ICHKOUT+1
    pha
    lda old_ICHKOUT
    pha
    jmp unmap
.endproc

.proc uclrcn
;    jsr is_our_device
;    bcc n
;    jsr enter
;    jsr _ultifs_kclrcn
;    jmp leave

n:  lda old_ICLRCN+1
    pha
    lda old_ICLRCN
    pha
    jmp unmap
.endproc

.proc ubasin
    jsr is_our_input
    bcc n
    jsr enter
    jsr _ultifs_kbasin
    jmp leave

n:  lda old_IBASIN+1
    pha
    lda old_IBASIN
    pha
    jmp unmap
.endproc

.proc ubsout
    jsr is_our_output
    bcc n
    jsr enter
    jsr _ultifs_kbsout
    jmp leave

n:  lda old_IBSOUT+1
    pha
    lda old_IBSOUT
    pha
    jmp unmap
.endproc

.proc uclall
    jsr enter
    jsr _ultifs_kclall
    lda old_ICLALL+1
    pha
    lda old_ICLALL
    pha
    jmp leave
.endproc

.proc uload
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_kload
    jmp leave

n:  lda old_ILOAD+1
    pha
    lda old_ILOAD
    pha
    jmp unmap
.endproc

.proc usave
    jsr is_our_device
    bcc n
    jsr enter
    jsr _ultifs_ksave
    jmp leave

n:  lda old_ISAVE+1
    pha
    lda old_ISAVE
    pha
    jmp unmap
.endproc
