gfx_init:
    ; Init VIC.
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

    ; Fill color RAM.
    lda #0
    tax
l:  sta colors,x
    sta @(+ 256 colors),x
    dex
    bne -l

    ; Make bitmap columns on screen.
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
n:  pla
    clc
    adc #1
    dex
    bne -m

    ; Fill background.
    lda #0
    sta xpos
    sta ypos
    lda #@(-- screen_width)
    sta width
    lda #screen_height
    sta height
    lda #<pat_background
    sta pattern
    lda #>pat_background
    sta @(++ pattern)
    jsr frect

    rts
