main:
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

    lda #%01111111
    sta $9ff2
    lda #0
    tax
    stx $9ff4
    sta $9ff5
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

    lda #<boot_end
    sta s
    lda #>boot_end
    sta @(++ s)
    lda #$00
    sta d
    sta c
    lda #$20
    sta @(++ d)
    sta @(++ c)
    lda #0
    jsr moveram
    jmp $2000
