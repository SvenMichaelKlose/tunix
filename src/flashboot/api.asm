.export set_banks
.export restore_banks
.export restore_banks_and_y

.data

; For some reasone __ZP_SIZE__ and related do not seem to be
; constants, so we go for all of the zero page.
saved_zp: .res 256

.code

.proc save_zp
    ldx #0
l:  lda 0,x
    sta saved_zp,x
    inx
    bne l
    rts
.endproc

.proc restore_zp
    ldx #0
l:  lda saved_zp,x
    sta 0,x
    inx
    bne l
    rts
.endproc

.proc set_banks
    and #%11111100
    ora #%00000011
    sta $9ff1
    lda #$7f
    sta $9ff4
    lda #$00
    sta $9ff5

    rts
.endproc

.proc restore_banks
    pla
    sta $9ff1
    pla
    sta $9ff5
    pla
    sta $9ff4

    rts
.endproc

.proc restore_banks_and_y
    pla
    sta $9ff1
    pla
    sta $9ff5
    pla
    sta $9ff4
    pla
    tay

    rts
.endproc
