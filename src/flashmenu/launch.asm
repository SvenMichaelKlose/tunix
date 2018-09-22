.export _launch
.importzp s, d, c
.import popax

bstart  = $2b       ; start of BASIC program text
bend    = $2d       ; end of basic program text
membot  = $282      ; start page of BASIC RAM
memtop  = $284      ; end page of BASIC RAM
screen  = $288      ; start page of text matrix

warmstt = $c7ae     ; BASIC warm start

ultimem_config = $9fe0

; extern void __fastcall__ launch (unsigned start, unsigned size);
.proc _launch
    sta c
    stx c+1
    jsr popax
    sta d
    stx d+1
    lda #0
    sta s
    lda #7
    sta $9ffe
l4: inc $9ffe
    lda #$a0
    sta s+1
    ldy #0
l:  lda (s),y
    sta (d),y
    inc d
    bne l2
    inc d+1
l2: inc s
    bne l3
    inc s+1
    lda s+1
    cmp #$c0
    beq l4
l3: dec c
    lda c
    cmp #$ff
    bne l
    dec c+1
    lda c+1
    cmp #$ff
    bne l

    lda #>$1000     ; A => screen at $1000
    ldx #>$1200     ; X => BASIC at $1200
    ldy #>$8000     ; Y => BASIC ends at $8000
    sty $c2         ; end of RAM at $8000
    sta screen
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

    jmp warmstt
.endproc

.proc reset
    ldx #$0f
l:  lda ultimem_config,x
    sta $9ff0,x
    dex
    bpl l
    jmp warmstt
.endproc
