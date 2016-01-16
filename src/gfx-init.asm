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

    ; Configure box to fill.
    lda #@(* 8 (-- screen_columns))
    sta xpos
    lda #@(* 16 screen_rows)
    sta height
    ldy #$ff
    sty masks
    iny
    sty maskd
    sty ypos

    ; Make pattern.
    ldx #6
l:  lda #$aa
    sta pattern,x
    lda #$55
    sta @(++ pattern),x
    dex
    dex
    bpl -l

    ; Fill screen column by column.
l:  jsr fill_column
    lda xpos
    sec
    sbc #8
    bcc +done
    sta xpos
    bcs -l
done:

    rts
