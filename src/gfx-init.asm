gfx_init:
    inc $9001
    inc $9001
    lda #22
    sta $9002
    lda #@(+ (* 2 11) 1)
    sta $9003
    lda #$ec    ; screen=$1800, chars=$1000
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
    lda #0
    sta $1800,x
    sta $1900,x
    lda #black
    sta $9400,x
    sta $9500,x
    dex
    bne -l
    rts
