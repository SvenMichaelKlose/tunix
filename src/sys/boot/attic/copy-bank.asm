.export init_copy_bank, copy_bank

.import lock_bank
.importzp s, d, c

first_bank = 120

OPCODE_LDA_ABS = $ad
OPCODE_STA_ABS = $8d
OPCODE_RTS = $60

.code

.proc init_copy_bank
    lda #first_bank
    sta $9ff8
    jsr lock_bank

    lda #$00
    sta s
    sta d
    lda #$20
    sta s+1
    sta d+1
    lda #$fe    ; Magic.
    sta c
    lda #$20
    sta c+1

    ldy #0
next:
    lda #OPCODE_LDA_ABS
    jsr out
    lda s
    jsr out
    lda s+1
    clc
    adc #$20
    jsr out
    lda #OPCODE_STA_ABS
    jsr out
    lda s
    jsr out
    lda s+1
    jsr out

    inc s
    bne l1
    inc s+1

l1: dec c
    bne next
    dec c+1
    bne next

    ; Restore BLK2.
    lda #3
    sta $9ff8
    rts

out:sta (d),y
    inc d
    beq l2

    lda d
    cmp #$fe
    bne l3
    lda d+1
    cmp #$3f
    bne l3

return:
    lda #OPCODE_RTS
    sta (d),y
    lda #$00
    sta d
    lda #$20
    sta d+1
    lda $9ff8
    clc
    adc #1
    sta $9FF8
    jmp lock_bank
    
l2: inc d+1
l3: rts
.endproc

.proc copy_bank
    lda $9ff2
    pha
    and #%11001111
    ora #%00110000
    sta $9ff2
    lda $9ffc
    pha
    lda $9ffd
    pha
    lda #first_bank
    sta $9ffc
    lda #0
    sta $9ffd

    ldx #6
l1: jsr $6000
    lda $9ffc
    clc
    adc #1
    sta $9ffc
    dex
    bne l1

    ; Copy missing 2 bytes.
    lda $5ffe
    sta $3ffe
    lda $5fff
    sta $3fff

    pla
    sta $9ffd
    pla
    sta $9ffc
    pla
    sta $9ff2
    rts
.endproc
