; UltiFS secondary wedge
;
; Banks in BLK2 and BLK3 and calls the kernal emulation
; or returns for calling regular KERNAL functions.


.export _init_secondary_wedge
.export uopen, uclose, uchkin, uckout, uclrcn
.export ubasin, ubsout, uclall, uload, usave

.import _ultifs_kopen, _ultifs_kclose, _ultifs_kchkin, _ultifs_kckout, _ultifs_kclrcn
.import _ultifs_kbasin, _ultifs_kbsout, _ultifs_kclall
.import _ultifs_kusrcmd, _ultifs_kload, _ultifs_ksave
.import unmap
.import _accu, _xreg, _yreg, _cfg, _proc_blk2, _proc_blk3, _proc_blk5
.import __ZP_START__
.import __ZP_SIZE__


zp_size  = $38    ; Keep greater or equal to what linker config file says.

.segment "SECONDARY"

IOPEN   = $031A

jmp to_cc65_startup

; .org $2000
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

_saved_zp:  .res zp_size

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
    ; Save BLK2, BLL3 and BLK5.
    lda $9ffa
    sta _proc_blk2
    lda $9ffb
    sta _proc_blk2+1
    lda $9ffc
    sta _proc_blk3
    lda $9ffd
    sta _proc_blk3+1
    lda $9ffe
    sta _proc_blk5
    lda $9fff
    sta _proc_blk5+1

    ; Bank in rest of UltiFS at BLK2 and BLK3.
    lda #$ff
    sta $9ff2
    ldy #118
    ldx #0
    sty $9ffa
    stx $9ffb
    iny
    sty $9ffc
    stx $9ffd

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
    lda _proc_blk2
    sta $9ffa
    lda _proc_blk2+1
    sta $9ffb
    lda _proc_blk3
    sta $9ffc
    lda _proc_blk3+1
    sta $9ffd
    lda _proc_blk5
    sta $9ffe
    lda _proc_blk5+1
    sta $9fff

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
    jsr enter
    jsr _ultifs_kclrcn
    jmp leave
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
