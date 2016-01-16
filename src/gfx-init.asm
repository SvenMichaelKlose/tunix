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
    lda #<pat_background
    sta pattern
    lda #>pat_background
    sta @(++ pattern)

    ; Fill screen column by column.
l:  jsr vfill
    lda xpos
    sec
    sbc #8
    bcc +done
    sta xpos
    bcs -l
done:

    lda #0
    sta xpos
    sta ypos
    lda #screen_width
    sta width
    lda #<pat_empty
    sta pattern
    lda #>pat_empty
    sta @(++ pattern)
l:  jsr hline
    inc ypos
    lda ypos
    cmp #screen_height
    bne -l

    lda #0
    sta xpos
    sta ypos
    lda #screen_height
    sta height
    lda #<pat_smiley
    sta pattern
    lda #>pat_smiley
    sta @(++ pattern)
l:  jsr vline
    inc xpos
    lda xpos
    cmp #screen_width
    bne -l

    lda #40
    sta xpos
    lda #0
    sta ypos
    lda #40
    sta height
    lda #40
    sta width
    lda #<pat_solid
    sta pattern
    lda #>pat_solid
    sta @(++ pattern)
l:  jsr rect
    inc xpos
    inc ypos
    lda xpos
    cmp #80
    bne -l

    rts

pat_empty:
    0 0 0 0 0 0 0 0

pat_solid:
    $ff $ff $ff $ff $ff $ff $ff $ff

pat_background:
    $aa $55 $aa $55 $aa $55 $aa $55 $aa $55

pat_smiley:
    %00111100
    %01000010
    %10100101
    %10100101
    %10000001
    %01011010
    %00100100
    %00011000
