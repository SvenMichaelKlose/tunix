gfx_init:
    inc $9000
    inc $9000
    inc $9001
    inc $9001
    lda #@(+ 128 screen_columns)
    sta $9002
    lda #@(+ (* 2 screen_rows) 1)
    sta $9003
    lda #$fc    ; screen=$1e00, chars=$1000
    sta $9005
    lda #@(+ (* white 16) reverse white)
    sta $900f

    ; Draw background pattern.
    lda #0
    tax
l:  txa
    lsr
    bcc +n
    lda #$55
    bne +m
n:  lda #$aa
m:  sta $1000,x
    sta $1100,x
    sta $1200,x
    sta $1300,x
    sta $1400,x
    sta $1500,x
    sta $1600,x
    sta $1700,x
    sta $1800,x
    sta $1900,x
    sta $1a00,x
    sta $1b00,x
    sta $1c00,x
    sta $1d00,x
    lda #0
    sta screen,x
    sta @(+ 256 screen),x
    lda #black
    sta colors,x
    sta @(+ 256 colors),x
    dex
    bne -l

    lda #<screen
    sta d
    lda #>screen
    sta @(++ d)
    ldx #11
    lda #@(* (-- screen_columns) screen_rows)

m:  pha
    ldy #@(-- screen_columns)

l:  sta (d),y
    sec
    sbc #screen_rows
    dey
    bpl -l

    lda d
    clc
    adc #screen_columns
    sta d
    bcc +n
    inc @(++ d)
n:
    pla
    clc
    adc #1
    dex
    bne -m

    rts
