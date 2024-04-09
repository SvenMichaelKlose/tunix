.import _ultimem_unhide

; Just start BASIC.
START_BASIC = 1

; The RAM bank to copy this ROM to,
; before executing the cc65 binary in
; it.
RAM_BANK    = 6

READY     := $C474
PRNTCRLF  := $CAD7
PTRSTR    := $CB1E
INITBA    := $E3A4
INITVCTRS := $E45B
FREMSG    := $E404
INITSK    := $E518
INITMEM   := $FD8D
FRESTOR   := $FD52
INITVIA   := $FDF9

    .zeropage

s:  .res 2
d:  .res 2
c:  .res 2

    .segment "RAMIFY"

    sei
    lda #$7f
    sta $911d
    sta $911e
    cld
    ldx #$ff
    txs

    jsr _ultimem_unhide

    lda #%00111111 ; IO 23
    sta $9ff1
    lda #%01111111
    sta $9ff2
    ldx #0
l0: lda ramify,x
    sta $9800,x
    dex
    bne l0
    jmp $9800

; Relocated up to 'continue'.
; No absolute jumps in here.
ramify:
    lda #0
    sta s
    sta d
    sta c
    lda #$a0
    sta s+1
    lda #$60
    sta d+1
    lda #$20
    sta c+1
    lda #RAM_BANK
    sta $9ffc

    ldy #0
    ldx c
    inx
    inc c+1
    bne copy_forwards ; (jmp)

l:  lda (s),y
    sta (d),y
    iny
    beq k
copy_forwards:
q:  dex
    bne l
    dec c+1
    bne l
    lda #%11111111
    sta $9ff2
    lda #RAM_BANK
    sta $9ffe
    jmp continue
k:  inc s+1
    inc d+1
    bne q   ; (jmp)

continue:

    jsr INITMEM     ; Init memory
    jsr FRESTOR     ; I/O vectors
    jsr INITVIA     ; VIAs
    jsr INITSK      ; VIC & clear screen

.ifdef START_BASIC
    jsr INITVCTRS   ; BASIC vectors
    jsr INITBA      ; BASIC zero page
    jsr FREMSG
    jmp READY
.endif ; .ifdef START_BASIC
