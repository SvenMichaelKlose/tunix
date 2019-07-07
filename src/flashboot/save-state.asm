.export save_state
.export restore_state

.importzp s, d, c, size ; TODO: Use c in copy.

.import popax
.import moveram
.import ultimem_copy_ram2ram
.import ultimem_offset2bank
.import ultimem_bank2offset
.import copy_bank

.proc copy_to_state
    sta $9ffa
    stx $9ffb
    ldx #d
    ldy #$08
    jsr ultimem_offset2bank
;    jsr ultimem_bank2offset
    jmp copy
.endproc

.proc copy_from_state
    sta $9ff8
    stx $9ff9
    ldx #s
    ldy #$0a
    jsr ultimem_offset2bank
;    jsr ultimem_bank2offset
    jmp copy
.endproc

.proc copy
    lda #$00
    sta size
    lda #$20
    sta size+1
    lda #$00
    sta size+2
    sta size+3
;    jsr ultimem_copy_ram2ram
    jmp copy_bank
.endproc

; Saves program state to $080000 in Ultimem RAM.
; Expects restart address at $0104.
; Expects Ultimem register set at $0120.
.proc save_state
    ; Write restore marker.
    lda #'S'
    sta $100
    lda #'T'
    sta $101
    lda #'A'
    sta $102
    lda #'E'
    sta $103

    ; Save VIC.
    ldx #$0f
l1: lda $9000,x
    sta $0110,x
    dex
    bpl l1

    ; Save bank config and BLK1.
    lda $9ff2
    pha
    lda $9ff8
    pha
    lda $9ff9
    pha
    lda $9ffa
    pha
    lda $9ffb
    pha
    lda $9ffc
    pha
    lda $9ffd
    pha

    ; Map in first bank of saved state.
    lda #%01111111
    sta $9ff2
    lda #$40
    sta $9ff8
    lda #$00
    sta $9ff9

    ; Save zeropage to $2000.
    ldx #0
l5: lda 0,x
    sta $2000,x
    dex
    bne l5
    ; Now we can use the zeropage ourselves.

    ; Save $0100-$03ff to $2100.
    lda #$00
    sta s
    ldy #$01
    sty s+1
    sta d
    ldy #$21
    sty d+1
    sta c
    ldy #$03
    sty c+1
    jsr moveram

    ; Save color RAM to $2400.
    lda #$00
    sta s
    ldy #$94
    sty s+1
    sta d
    ldy #$24
    sty d+1
    sta c
    ldy #$04
    sty c+1
    jsr moveram

    ; Save $1000-$1fff to $3000.
    lda #$00
    sta s
    ldy #$10
    sty s+1
    sta d
    ldy #$30
    sty d+1
    sta c
    ldy #$10
    sty c+1
    jsr moveram

    ; Save RAM1,2,3.
    lda #0
    sta d
    ldy #$20
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $0124       ; (saved Ultimem reg)
    ldx $0125
    jsr copy_to_state

    ; Save IO2/IO3.
    lda #0
    sta d
    ldy #$40
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $0126
    ldx $0127
    jsr copy_to_state

    ; Save BLK1.
    lda #0
    sta d
    ldy #$60
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $0128
    ldx $0129
    jsr copy_to_state

    ; Save BLK2.
    lda #0
    sta d
    ldy #$80
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $012a
    ldx $012b
    jsr copy_to_state

    ; Save BLK3.
    lda #0
    sta d
    ldy #$a0
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $012c
    ldx $012d
    jsr copy_to_state

    ; Save BLK5.
    lda #0
    sta d
    ldy #$c0
    sty d+1
    ldy #$08
    sty d+2
    sta d+3
    lda $012e
    ldx $012f
    jsr copy_to_state

    ; Restore zeropage which we might have destroyed.
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
    sta $9ffd
    pla
    sta $9ffc
    pla
    sta $9ffb
    pla
    sta $9ffa
    pla
    sta $9ff9
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
    lda #0
    sta $9ff9

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

    lda #$00    ; Blank, yellow screen.
    sta $9002
    lda #$7f
    sta $900f

    ; Restore $0200-$03ff.
    lda #0
    sta d
    ldy #$02
    sty d+1
    sta s
    ldy #$22
    sty s+1
    sta c
    ldy #$02
    sty c+1
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

    ; Decrement restart address.
    dec $0104
    lda $0104
    cmp #$ff
    bne n
    dec $0105
n:

    ; Restore color RAM $9400-$97ff.
    lda #0
    sta d
    ldy #$94
    sty d+1
    sta s
    ldy #$24
    sty s+1
    sta c
    ldy #$04
    sty c+1
    jsr moveram

    ; Restore $1000-$1fff.
    lda #0
    sta d
    ldy #$10
    sty d+1
    sta s
    ldy #$30
    sty s+1
    sta c
    ldy #$10
    sty c+1
    jsr moveram

    lda #0
    sta s
    ldy #$20
    sty s+1
    ldy #$08
    sty s+2
    sta s+3
    lda $0124
    lda $0125
    jsr copy_from_state   ; RAM1,2,3

    lda #0
    sta s
    ldy #$40
    sty s+1
    ldy #$08
    sty s+2
    sta s+3
    lda $0126
    ldx $0127
    jsr copy_from_state   ; IO1,2,3

    lda #0
    sta s
    ldy #$60
    sty s+1
    ldy #$08
    sty s+2
    sta s+3
    lda $0128
    ldx $0129
    jsr copy_from_state   ; BLK1

    lda #0
    sta s
    ldy #$80
    sty s+1
    ldy #$08
    sty s+2
    sta s+3
    lda $012a
    ldx $012b
    jsr copy_from_state   ; BLK2

    lda #0
    sta s
    ldy #$a0
    sty s+1
    ldy #$08
    sty s+2
    sta s+3
    lda $012c
    ldx $012d
    jsr copy_from_state   ; BLK3

    lda #0
    sta s
    ldy #$c0
    sty s+1
    ldy #$08
    sty s+2
    sta s+3
    lda $012e
    ldx $012f
    jsr copy_from_state   ; BLK5

    ; Make and call trampoline.
    ldx #restart_trampoline_end-restart_trampoline_start
l2: lda restart_trampoline-1,x
    sta $180-1,x
    dex
    bne l2
    jmp $180

restart_trampoline:
    .org $180

restart_trampoline_start:
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

    ; Restore VIC.
    ldx #$0f
l1: lda $2110,x
    sta $9000,x
    dex
    bpl l1

    ; Restore Ultimem.
    ldx #$0f
l4: lda $0120,x
    sta $9ff0,x
    dex
    bpl l4

    ; Call restart function.
    lda $105
    pha
    lda $104
    pha
    jsr $fdf9   ; Init VIAs.
    cli
    rts
restart_trampoline_end:
.endproc
