.export _launch
.importzp s, d, c, tmp, tmp2
.import popax

bstart  = $2b       ; start of BASIC program text
bend    = $2d       ; end of basic program text
membot  = $282      ; start page of BASIC RAM
memtop  = $284      ; end page of BASIC RAM
screen  = $288      ; start page of text matrix

warmstt = $c7ae     ; BASIC warm start

; void __fastcall__ launch (unsigned start, unsigned size);
.proc _launch
    .org $9800
    sta c
    stx c+1
    jsr popax
    sta d
    sta tmp
    stx d+1
    stx tmp2
    lda #0
    sta s

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

    lda #%10000000  ; Hide registers, LED off.
    sta $9ff0

    lda #>warmstt
    pha
    lda #<warmstt
    pha
    lda #0
    pha
    rti

    ; Copy loaded data starting at bank 8 to RAM via BLK5.
copy_loaded_to_ram:
    lda #7
    sta $9ffe
    ldy #0
    ldx c
l4: inc $9ffe
    lda #>$a000
    sta s+1
l:  lda (s),y
    sta (d),y
    inc d
    beq d1
l2: inc s
    beq d2
l3: dex
    cpx #$ff
    bne l
    dec c+1
    lda c+1
    cmp #$ff
    bne l
    rts

d1: inc d+1
    bne l2
    beq l2

d2: inc s+1
    lda s+1
    cmp #>$c000
    beq l4
    bne l3
.endproc
