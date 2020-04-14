.export set_banks
.export restore_banks
.export restore_banks_and_y

SAVED_ZP_SIZE = 64

.data

saved_zp: .res SAVED_ZP_SIZE

.code

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
