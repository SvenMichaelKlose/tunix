.export main
.exportzp s, d, c

.import moveram, __PRGEND__

s = 0
d = 2
c = 4

.segment "STARTUP"

main:
    ; Don't get interrupted.
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init VIC.

    ; Activate all RAM.
    lda #%00111111
    sta $9ff1
    lda #%01011111
    sta $9ff2
    lda #0
    tax
    stx $9ff4
    sta $9ff5
    inx
    stx $9ff6
    sta $9ff7
    inx
    stx $9ff8
    sta $9ff9
    inx
    stx $9ffa
    sta $9ffb
    inx
    stx $9ffc
    sta $9ffd
    cli

    ; Make dummy call to g's link().
    lda #$60	; RTS
    sta $0400

    ;; Copy first 16K of second 64K flash segment to RAM.
    ; First 8K – skips 6 byte program header.
    lda #$08
    sta $9ffc
    lda #$06
    sta s
    lda #$60
    sta s+1
    lda #$00
    sta d
    lda #$20
    sta d+1
    lda #$fa
    sta c
    lda #$1f
    sta c+1
    lda #0
    jsr moveram

    inc $9ffc
    lda #$00
    sta s
    lda #$60
    sta s+1
    lda #$fa
    sta d
    lda #$3f
    sta d+1
    lda #$00
    sta c
    lda #$20
    sta c+1
    lda #0
    jsr moveram

    lda #%01111111
    sta $9ff2

    ; Run it.
    jmp $2000

dummy_link:
    rts
