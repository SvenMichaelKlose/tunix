.export save_state
.export restore_state
.importzp s, d, c, ptr, size
.import popax
.import ultimem_copy_ram2ram
.import moveram

.proc bank2ptr
    ; Get offset of bank # in A.
    ldx #0
    stx ptr+0
    stx ptr+1
    stx ptr+2
    stx ptr+3
    ldx #5
l1: asl
    rol ptr+2
    dex
    bne l1
    sta ptr+1
    rts
.endproc

.proc bank2d
    ; Get offset of bank # in A.
    ldx #0
    stx d+0
    stx d+1
    stx d+2
    stx d+3
    ldx #5
l1: asl
    rol d+2
    dex
    bne l1
    sta d+1
    rts
.endproc

.proc copy_bank
    lda #$00
    sta size
    lda #$20
    sta size+1
    lda #$00
    sta size+2
    sta size+3
    jmp ultimem_copy_ram2ram
.endproc

; Saves program state to $080000 in Ultimem RAM.
.proc save_state
    ; Save return address.
    sta $104
    stx $105

    ; Write restore marker.
    lda #'S'
    sta $100
    lda #'T'
    sta $101
    lda #'A'
    sta $102
    lda #'E'
    sta $103

    ; Save VIC and Ultimem.
    ldx #$0f
l1: lda $9000,x
    sta $0110,x
    lda $9ff0,x
    sta $0120,x
    dex
    bpl l1

    ; Save internal RAM + RAM1,2,3.
    lda $9ff2
    pha
    lda $9ff8
    pha
    lda #%01111111
    sta $9ff2
    lda #$40
    sta $9ff8

    ; (Save zeropage without modifying it.)
    ldx #0
l5: lda 0,x
    sta $2000,x
    dex
    bne l5

    ; Save $0100-$1fff
    lda #$00
    sta s
    ldy #$01
    sty s+1
    sta d
    ldy #$21
    sty d+1
    sta c
    ldy #$1f
    sty c+1
    lda #0
    jsr moveram

    ; Save color RAM.
    inc $9ff8
    ldx #0
l7: lda $9400,x
    sta $2000,x
    lda $9500,x
    sta $2100,x
    lda $9600,x
    sta $2200,x
    lda $9700,x
    sta $2300,x
    dex
    bne l7

    lda #0
    sta d
    ldy #$40
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $0124
    jsr bank2ptr
    jsr copy_bank   ; RAM1,2,3

    lda #0
    sta d
    ldy #$60
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $0126
    jsr bank2ptr
    jsr copy_bank   ; IO2/IO3

    lda #0
    sta d
    ldy #$80
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $0128
    jsr bank2ptr
    jsr copy_bank   ; BLK1

    lda #0
    sta d
    ldy #$a0
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $012a
    jsr bank2ptr
    jsr copy_bank   ; BLK2

    lda #0
    sta d
    ldy #$c0
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $012c
    jsr bank2ptr
    jsr copy_bank   ; BLK3

    lda #0
    sta d
    ldy #$e0
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $012e
    jsr bank2ptr
    jsr copy_bank   ; BLK5

    ; Restore zeropage.
    lda #%01111111
    sta $9ff2
    lda #$40
    sta $9ff8
    ldx #0
l6: lda $2000,x
    sta $0,x
    dex
    bne l6

    pla
    sta $9ff8
    pla
    sta $9ff2
    rts
.endproc

; Saves program state to $080000 in Ultimem RAM.
.proc restore_state
    ; Map first bank of state to $2000.
    lda #%01111111
    sta $9ff2
    lda #$40
    sta $9ff8

    ; Check on restore marker.
    lda $2100
    cmp #'S'
    bne r
    lda $2101
    cmp #'T'
    bne r
    lda $2102
    cmp #'A'
    bne r
    lda $2103
    cmp #'E'
    beq l
r:  rts
l:

    ; Restore VIC.
    ldx #$0f
l1: lda $2110,x
    sta $9000,x
    dex
    bpl l1

    ; Restore $0200-$1fff.
    lda #%01111111
    sta $9ff2
    lda #$40
    sta $9ff8
    lda #0
    sta d
    ldy #$02
    sty d+1
    sta s
    ldy #$22
    sty s+1
    sta c
    ldy #$1e
    sty c+1
    lda #0
    jsr moveram

    ; Copy Ultimem.
    ldx #$0f
l6: lda $2120,x
    sta $0120,x
    dex
    bpl l6

    ; Copy restart address.
    lda $2104
    sta $0104
    lda $2105
    sta $0105
    dec $0104
    lda $0104
    cmp #$ff
    bne n
    dec $0105
n:

    ; Restore color RAM.
    inc $9ff8
    ldx #0
l7: lda $2000,x
    sta $9400,x
    lda $2100,x
    sta $9500,x
    lda $2200,x
    sta $9600,x
    lda $2300,x
    sta $9700,x
    dex
    bne l7

    lda #0
    sta ptr
    ldy #$40
    sty ptr+1
    ldy #$08
    sty ptr+2
    sta ptr+3
    lda $0124
    jsr bank2d
    jsr copy_bank   ; RAM1,2,3

    lda #0
    sta ptr
    ldy #$60
    sty ptr+1
    ldy #$08
    sty ptr+2
    sta ptr+3
    lda $0126
    jsr bank2d
    jsr copy_bank   ; IO1,2,3

    lda #0
    sta ptr
    ldy #$80
    sty ptr+1
    ldy #$08
    sty ptr+2
    sta ptr+3
    lda $0128
    jsr bank2d
    jsr copy_bank   ; BLK1

    lda #0
    sta ptr
    ldy #$a0
    sty ptr+1
    ldy #$08
    sty ptr+2
    sta ptr+3
    lda $012a
    jsr bank2d
    jsr copy_bank   ; BLK2

    lda #0
    sta ptr
    ldy #$c0
    sty ptr+1
    ldy #$08
    sty ptr+2
    sta ptr+3
    lda $012c
    jsr bank2d
    jsr copy_bank   ; BLK3

    lda #0
    sta ptr
    ldy #$e0
    sty ptr+1
    ldy #$08
    sty ptr+2
    sta ptr+3
    lda $012e
    jsr bank2d
    jsr copy_bank   ; BLK5

    ; Make and call trampoline.
    ldx #$30
l2: lda restart_trampoline,x
    sta $180,x
    dex
    bpl l2
    jmp $180

restart_trampoline:
    .org $180

    ; Restore zeropage.
    lda #%01111111
    sta $9ff2
    lda #$40
    sta $9ff8
    ldx #0
l3: lda $2000,x
    sta $0,x
    dex
    bne l3

    ; Restore Ultimem.
    ldx #$0f
l4: lda $0120,x
    sta $9ff0,x
    dex
    bpl l4

    ; TODO: Remove workaround.
    lda #1
    sta $9ff8

    ; Call restart function.
    lda $105
    pha
    lda $104
    pha
    cli
    rts
.endproc
