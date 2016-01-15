gfx_init:
    lda #@(+ 128 22)
    sta $9002
    lda #$ec    ; screen=$1800, chars=$1000
    sta $9005
    lda #@(+ (* white 16) reverse white)
    sta $900f

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
    dex
    bne -l
    rts
