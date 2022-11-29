; UltiFS secondary wedge
;
; Banks in BLK2 and BLK3 and calls the kernal emulation
; or returns for calling regular KERNAL functions.


.export _init_secondary_wedge
.export uopen, uclose, uchkin, uckout, uclrcn
.export ubasin, ubsout, uclall, uload, usave

.import _ultifs_kopen, _ultifs_kclose, _ultifs_kchkin, _ultifs_kckout
.import _ultifs_kbasin, _ultifs_kbsout, _ultifs_kclall
.import _ultifs_kusrcmd, _ultifs_kload, _ultifs_ksave
.import unmap
.import _accu, _xreg, _yreg, _cfg, _blk2, _blk3, _blk5
.import __ZP_START__
.import __ZP_SIZE__


stack_size  = $22    ; Keep greater or equal to what linker config file says.

.segment "SECONDARY"

IOPEN   = $031A

jmp to_cc65_startup

.export drv_ultifs_vectors
drv_ultifs_vectors:
    .word uopen
    .word uclose
    .word uchkin
    .word uckout
    .word uclrcn
    .word ubasin
    .word ubsout
    .word 0         ;ustop
    .word 0         ;ugetin
    .word uclall
    .word 0         ;uusr
    .word uload
    .word usave

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

_last_regular_device:   .res 1
_last_ingle_device:     .res 1

_saved_zp:  .res stack_size

.proc _init_secondary_wedge
    ; Save zeropage.
    ldx #<__ZP_SIZE__
l2: lda __ZP_START__-1,x
    sta _saved_zp-1,x
    dex
    bne l2

    rts
.endproc

enter:
    sty _yreg
    stx _xreg

    ; Save BLK2, BLL3 and BLK5.
    lda $9ffa
    sta _blk2
    lda $9ffb
    sta _blk2+1
    lda $9ffc
    sta _blk3
    lda $9ffd
    sta _blk3+1
    lda $9ffe
    sta _blk5
    lda $9fff
    sta _blk5+1

    ; Bank in rest of UltiFS at BLK2 and BLK3.
    lda #$ff
    sta _cfg
    lda #118
    sta _blk2
    lda #119
    sta _blk3
    lda #0
    sta _blk2+1
    sta _blk3+1

.proc swap_zp
    ; Swap zeropage.
    ldx #<__ZP_SIZE__
l:  lda __ZP_START__-1,x
    ldy _saved_zp-1,x
    sta _saved_zp-1,x
    tya
    sta __ZP_START__-1,x
    dex
    bne l

    rts
.endproc

.proc leave
    jsr swap_zp

    ; Restore BLK2, BLK3 and BLK5.
    lda _blk2
    sta $9ffa
    lda _blk2+1
    sta $9ffb
    lda _blk3
    sta $9ffc
    lda _blk3+1
    sta $9ffd
    lda _blk5
    sta $9ffe
    lda _blk5+1
    sta $9fff

    ; Restore X and Y register.  Accu, flags and BLK1
    ; will be restored by unmap().
    ldx _xreg
    ldy _yreg
    jmp unmap
.endproc

.proc uopen
    jsr enter
    jsr _ultifs_kopen
    jmp leave
.endproc

.proc uchkin
    jsr enter
    jsr _ultifs_kchkin
    jmp leave
.endproc

.proc uckout
    jsr enter
    jsr _ultifs_kckout
    jmp leave
.endproc

.proc ubasin
    jsr enter
    jsr _ultifs_kbasin
    jmp leave
.endproc

.proc ubsout
    jsr enter
    jsr _ultifs_kbsout
    jmp leave
.endproc

.proc uclrcn
    rts
.endproc

.proc uclose
    jsr enter
    jsr _ultifs_kclose
    jmp leave
.endproc

.proc uclall
    jsr enter
    jsr _ultifs_kclall
    jmp leave
.endproc

.proc uload
    jsr enter
    jsr _ultifs_kload
    jmp leave
.endproc

.proc usave
    jsr enter
    jsr _ultifs_ksave
    jmp leave
.endproc

to_cc65_startup:
