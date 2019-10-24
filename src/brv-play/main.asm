.export _main

.import pushax
.import ultimem_unhide_regs
.import clrram

.importzp s, d, c

screen = $1000
colors = $9400

.data

first_bank = 20
screen_columns: .res 1
screen_rows: .res 1
tmp: .res 1
tmp2: .res 1

.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e

    jsr ultimem_unhide_regs

    lda #<txt_installing
    ldy #>txt_installing
    jsr $cb1e

    lda #%11111111
    sta $9ff2
    lda #first_bank
    sta $9ffe
    lda #0
    sta $9fff

    lda #$00
    sta d
    lda #$a0
    sta d+1

    lda #2
    ldx #8
    ldy #2
    jsr $ffba   ; SETLF
    lda #fn_data_end-fn_data
    ldx #<fn_data
    ldy #>fn_data
    jsr $ffbd
    jsr $ffc0
    ldx #2
    jsr $ffc6

    lda #$00
    sta d
    lda #$a0
    sta d+1

l:  jsr $ffcf
    pha
    lda $90
    cmp #1
    pla
    bcs done_loading
    ldy #0
    sta (d),y
    inc d
    bne l
    inc d+1
    lda d+1
    cmp #$c0
    bne l
    lda #$a0
    sta d+1
    inc $9ffe
    jmp l

done_loading:
    jsr $ffcc

    lda #2
    jsr $ffc3

    lda #first_bank
    sta $9ffe
    lda #0
    sta $9fff

    lda $a000
    sta screen_columns
    lda $a001
    sta screen_rows

    lda #$a0    ; Aux light red
    sta $900e
    lda #9      ; Text white multicolour
    ldx #$20    ; Screen red, border black
    jsr init_bitmap_mode

    lda #%00000000
    sta $1100
    sta $1101
    sta $1102
    sta $1103
    lda #%01010101
    sta $1104
    sta $1105
    sta $1106
    sta $1107
    lda #%10101010
    sta $1108
    sta $1109
    sta $110a
    sta $110b
    lda #%11111111
    sta $110c
    sta $110d
    sta $110e
    sta $110f
;w:jmp w
    lda #$04
    sta s
    lda #$a0
    sta s+1

next_frame:
    lda $9004
    bne next_frame
l4: lda $9004
    beq l4
    jsr fetch
    sta c
    jsr fetch
    sta c+1
    cmp #$fe
done:
    beq done
    and c
    cmp #$ff
    beq next_frame

next_byte:
    jsr fetch       ; Fetch address.
    sta self+1
    jsr fetch
    sta self+2
    jsr fetch       ; Fetch colour.
self:
    sta $1234

countdown:
    dec c
    lda c
    cmp #255
    bne next_byte
    dec c+1
    lda c+1
    cmp #255
    bne next_byte

    jmp next_frame
    rts
.endproc

.proc fetch
    ldy #0
    lda (s),y
    inc s
    beq l
    rts

l:  inc s+1
    ldy s+1
    cpy #$c0
    beq n
    rts

n:  ldy #$a0
    sty s+1
    inc $9ffe
    rts
.endproc

.proc fill_colors
    ldx #0
l:  sta colors,x
    sta colors+256,x
    dex
    bne l
    rts
.endproc

; A: text colour
; X: screen colour / reverse / border colour
.proc init_bitmap_mode
    stx $900f

    ; Fill color RAM.
    jsr fill_colors

    ; Make bitmap columns on screen.
    lda #<screen
    sta s
    lda #>screen
    sta s+1
    lda screen_columns
    sta c+1
    ldx #$10

l2: lda s
    sta d
    lda s+1
    sta d+1
    lda screen_rows
    sta c

    ldy #0
l:  txa
    sta (d),y
    inx
    lda d
    clc
    adc screen_columns
    sta d
    lda d+1
    adc #0
    sta d+1
    dec c
    bne l

    inc s
    bne l3
    inc s+1

l3: dec c+1
    bne l2

    ; Initialise VIC.
    ldx $ede4
    inx
    inx
    stx $9000
    ldx $ede5
    dex
    stx $9001
    lda screen_columns
    sta $9002
    lda screen_rows
    asl
    ora #1
    sta $9003
    lda #$cc
    sta $9005

    rts
.endproc

.data

txt_welcome:
    .byte $93, "BRV PLAYER", 13, 0

txt_installing:
    .byte "LOADING VIDEO", 13, 0

fn_data:
    .byte "VIDEO.BRV,S,R"
fn_data_end:
