.export _init_hooks
.export _global_lfns
.export _accu, _xreg, _yreg, _flags
.export _cfg, _proc_blk1, _proc_blk2, _proc_blk3, _proc_blk5
.export unmap

.import __IO_LOAD__
.importzp s, d, c

    .code

new_vectors:
    .word h_open, h_close,h_chkin, h_ckout, h_clrcn
    .word h_basin, h_bsout, h_stop, h_getin, h_clall
    .word h_user, h_load, h_save

    .export h_open, h_close, h_chkin, h_ckout, h_clrcn
    .export h_basin, h_bsout, h_stop, h_getin, h_clall
    .export h_user, h_load, h_save

.proc _init_hooks
    ldx #0
    lda #$ff
l:  sta $9800,x
    inx
    bne l

    lda #$00
    sta d
    lda #$99
    sta d+1
    lda #<(__IO_LOAD__ + $100)
    sta s
    lda #>(__IO_LOAD__ + $100)
    sta s+1
    ldx #<(end_of_hooks-hooks)
    inx ; TODO: Put into expressin above.
    ldy #>(end_of_hooks-hooks)
    iny
    sty c+1

    ldy #0
l2: lda (s),y
    sta (d),y
    iny
    bne n
    inc s+1
    inc d+1
n:  dex
    bne l2
    dec c+1
    bne l2

    ldx #25
l3: lda $031a,x
    sta old_open,x
    lda new_vectors,x
    sta $031a,x
    dex
    bpl l3

    rts
.endproc

LFN     = $b8   ; Logical file number.
FA      = $ba   ; Device number
DFLTN   = $99   ; Current input device number.
DFLTO   = $9a   ; Current output device number.

    .segment "IO"
    .org $9800

_global_lfns:   .res 256
hooks:
_accu:          .res 1
_xreg:          .res 1
_yreg:          .res 1
_flags:         .res 1
_cfg:           .res 1
_proc_blk1:     .res 2
_proc_blk2:     .res 2
_proc_blk3:     .res 2
_proc_blk5:     .res 2

tmp:            .res 2

.enum
    IDX_OPEN = 0
    IDX_CLOSE
    IDX_CHKIN
    IDX_CKOUT
    IDX_CLRCN
    IDX_BASIN
    IDX_BSOUT
    IDX_STOP
    IDX_GETIN
    IDX_CLALL
    IDX_USER
    IDX_LOAD
    IDX_SAVE
.endenum

old_open:   .res 2
old_close:  .res 2
old_chkin:  .res 2
old_ckout:  .res 2
old_clrcn:  .res 2
old_basin:  .res 2
old_bsout:  .res 2
old_stop:   .res 2
old_getin:  .res 2
old_clall:  .res 2
old_user:   .res 2
old_load:   .res 2
old_save:   .res 2

; A: Vector index
; Y: Device number
call_driver:
    sta tmp
    php
    pla
    sta _flags

    lda $9ff2
    sta _cfg
    lda $9ff8
    sta _proc_blk1
    lda $9ff9
    sta _proc_blk1+1

    lda #117
    sta $9ff8
    lda #0
    sta $9ff9
    lda #$ff
    sta $9ff2

    lda tmp
    asl
    sta j+1
j:  jmp ($2000)

unmap:
    ; Restore memory config.
    lda _proc_blk1
    sta $9ff8
    lda _proc_blk1+1
    sta $9ff9
    lda _cfg
    sta $9ff2

    ; Update registers.
    lda _flags
    pha
    ldx _xreg
    ldy _yreg
    lda _accu
    plp
    rts

.proc h_open
    lda FA
    cmp #12
    bne not_us
    lda #IDX_OPEN
    jmp call_driver
not_us:
    jmp (old_open)
.endproc

.proc h_clrcn
    stx _xreg
    lda #IDX_CLRCN
    jsr call_driver
    ldx _xreg
    jmp (old_clrcn)
.endproc

.proc h_close
    sta _accu
    stx _xreg
    sty _yreg
    tay
    lda _global_lfns,y
    bmi not_us
    lda #IDX_CLOSE
    jmp call_driver
not_us:
    lda _accu
    ldy _yreg
    jmp (old_close)
.endproc

.proc h_chkin
    stx _xreg
    sty _yreg
    lda _global_lfns,x
    bmi not_us
    lda #IDX_CHKIN
    jmp call_driver
not_us:
    jmp (old_chkin)
.endproc

.proc h_ckout
    stx _xreg
    sty _yreg
    lda _global_lfns,x
    bmi not_us
    lda #IDX_CKOUT
    jmp call_driver
not_us:
    jmp (old_ckout)
.endproc

.proc h_basin
    sty _yreg
    ldy DFLTN
    lda _global_lfns,y
    bmi not_us
    lda #IDX_BASIN
    jmp call_driver
not_us:
    ldy _yreg
    jmp (old_basin)
.endproc

.proc h_bsout
    sta _accu
    sty _yreg
    ldy DFLTO
    lda _global_lfns,y
    bmi not_us
    lda #IDX_BSOUT
    jmp call_driver
not_us:
    lda _accu
    ldy _yreg       ; Fun fact: needs to be restored. (pixel)
    jmp (old_bsout)
.endproc

.proc h_stop
    jmp (old_stop)
.endproc

.proc h_getin
    jmp (old_getin)
.endproc

.proc h_user
    jmp (old_user)
.endproc

.proc h_clall
    lda #IDX_CLALL
    jsr call_driver
    jmp (old_clall)
.endproc

.proc h_load
    sta _accu
    lda FA
    cmp #12
    bne not_us
    stx _xreg
    sty _yreg
    lda #IDX_LOAD
    jmp call_driver
not_us:
    lda _accu
    jmp (old_load)
.endproc

.proc h_save
    sta _accu
    lda FA
    cmp #12
    bne not_us
    stx _xreg
    sty _yreg
    lda #IDX_SAVE
    jmp call_driver
not_us:
    lda _accu
    jmp (old_save)
.endproc

end_of_hooks:
