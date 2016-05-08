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
    lda #%01111111
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

    ; Copy following code in Flash to $2000 (skip first 4 header bytes).
    lda #<__PRGEND__
    sta s
    lda #>__PRGEND__
    sta s+1
    lda #$fa
    sta d
    sta c
    lda #$1f
    sta d+1
    sta c+1
    lda #0
    jsr moveram

    ; Run it.
    jmp $2000

dummy_link:
    rts
