.export launch
.exportzp tmp2
.importzp s, d, c, tmp
.import popax

bstart  = $2b       ; start of BASIC program text
bend    = $2d       ; end of basic program text
membot  = $282      ; start page of BASIC RAM
memtop  = $284      ; end page of BASIC RAM
screen  = $288      ; start page of text matrix

warmstt = $c7ae     ; BASIC warm start

.zeropage

tmp2:   .res 1

.code

.proc launch
    lda d
    sta tmp
    lda d+1
    sta tmp2

    ; Don't get interrupted.
    sei
    lda #$7f
    sta $911d
    sta $911e

    lda #0      ; Blank screen.
    sta $9002

    jsr copy_loaded_to_ram

    lda #$20
    sta $c2         ; I/O start addresses high byte.

    lda tmp2
    cmp #>$1000
    bne l7

    lda #>$1e00     ; screen
    ldx #>$1000     ; BASIC
    ldy #>$1e00     ; BASIC end
    bne l6

l7: bcs l5

    ; +3K
    lda #>$1e00     ; screen
    ldx #>$0400     ; BASIC
    ldy #>$1e00     ; BASIC end
    bne l6

    ; +24/32/35
l5: lda #$80
    sta $c2         ; I/O start addresses high byte.
    lda #>$1000     ; screen
    ldx #>$1200     ; BASIC
    ldy #>$8000     ; BASIC end

l6: sta screen
    stx membot
    sty memtop

    jsr $ff8a       ; initialize the KERNAL jump vectors
    jsr $fdf9       ; initialize the I/O chips
    jsr $e518       ; initialize the screen
    jsr $e45b       ; initialize jump vectors for BASIC
    jsr $e3a4       ; initialize zero page for BASIC
    lda bstart
    ldy bstart+1
    jsr $c408       ; check memory overlap
    jsr $c659       ; CLR

    ldx #trampoline_end-trampoline-1
l8: lda trampoline,x
    sta $33c,x
    dex
    bpl l8
    jmp $33c

trampoline:
    ; Map RAM banks.
    lda #%11111111
    sta $9ff2
    ldx #$02
    ldy #$00
    stx $9ffa
    sty $9ffb
    inx
    stx $9ffc
    sty $9ffd
    inx
    stx $9ffe
    sty $9fff

    lda #%10000000  ; Hide registers, LED off.
    sta $9ff0

    lda #>warmstt
    pha
    lda #<warmstt
    pha
    lda #0
    pha
    rti
trampoline_end:
.endproc

    ; Copy loaded data starting at bank 12 to RAM via BLK5.
.proc copy_loaded_to_ram
    lda #%00111111
    sta $9ff1
    lda #%01111111
    sta $9ff2
    ldy #0
    sty $9ff4
    sty $9ff5
    sty $9ff6
    sty $9ff7
    lda #1
    sta $9ff8
    sty $9ff9
    lda #11
    sta $9ffa
    sty $9ffb

    sty s
    ldx c
    inx
    inc c+1
l4: inc $9ffa
    lda #>$4000
    sta s+1
l:  lda (s),y
    sta (d),y
    inc d
    beq d1
l2: inc s
    beq d2
l3: dex
    bne l
    dec c+1
    bne l

    rts

d1: inc d+1
    lda d+1
    cmp #>$4000
    bne l2
    inc $9ff8
    lda #>$2000
    sta d+1
    bne l2  ; (jmp)

d2: inc s+1
    lda s+1
    cmp #>$6000
    beq l4
    bne l3  ; (jmp)
.endproc
