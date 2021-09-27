.export _term_init, _term_put

.import init_bitmap_mode, gfx_init
.import clear_screen
.import box
.import putchar_fixed
.import _pattern_solid
.import init_region_stack
.import reset_region

.importzp s, d, c, scrbase
.importzp font, pencil_mode, pattern
.importzp xpos, ypos, width, height

    .zeropage

tmp:        .res 1
tmp2:       .res 1
p:          .res 2
cursor_x:   .res 1
cursor_y:   .res 1
has_cursor:     .res 1
visible_cursor: .res 1

    .code

.proc _term_init
    jsr clear_screen
    lda #1
    ldx #0
    ldy #0
    jsr init_bitmap_mode
    lda #$00
    sta scrbase
    lda #$11
    sta scrbase+1

    jsr init_region_stack
    jsr reset_region

    lda #<charset
    sta font
    sta p
    lda #>charset
    sta font+1
    sta p+1

    ; Double each char's half.
    ldy #0
l2: lda (p),y
    sta tmp
    asl
    asl
    asl
    asl
    ora tmp
    sta (p),y
    iny
    bne l2
    inc p+1
    lda p+1
    cmp #>charset+8
    bne l2

    ldy #0
    sty cursor_x
    sty cursor_y
    sty visible_cursor
    iny
    sty has_cursor
    jsr cursor_enable

    lda #<txt_welcome
    sta p
    lda #>txt_welcome
    sta p+1
    jsr putstring_fixed

    jmp print_charset
.endproc

.proc cursor_draw
    lda has_cursor
    beq r

    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos
    lda #<_pattern_solid
    sta pattern
    lda #>_pattern_solid
    sta pattern+1
    lda #2
    sta pencil_mode
    asl
    sta width
    asl
    sta height
    jmp box

r:  rts
.endproc

.proc cursor_enable
    lda visible_cursor
    inc visible_cursor
    ora #0
    bne r
    jmp cursor_draw
r:  rts
.endproc

.proc cursor_disable
    dec visible_cursor
    lda visible_cursor
    bne r
    jmp cursor_draw
r:  rts
.endproc

.proc cursor_step
    lda cursor_x
    clc
    adc #4
    sta cursor_x

    cmp #160
    bne n

    lda #0
    sta cursor_x
    jmp cursor_down

n:  rts
.endproc

.proc cursor_down
    lda cursor_y
    clc
    adc #8
    sta cursor_y
    rts
.endproc

.proc line_break
    lda #0
    sta cursor_x
    jmp cursor_down
.endproc

.proc set_cursor_pos
    pha
    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos
    pla
    rts
.endproc

.proc _term_put
    pha
    pha
    jsr cursor_disable
    jsr set_cursor_pos
    pla
    jsr putchar_fixed
    jsr cursor_step
    jsr cursor_enable
    pla
    rts
.endproc

.proc putstring_fixed
    jsr cursor_disable

l:  ldy #0
    lda (p),y
    beq done

    cmp #13
    bne n
    jsr line_break
    jmp next

n:  jsr set_cursor_pos
    jsr putchar_fixed
    jsr cursor_step

next:
    inc p
    bne l
    inc p+1
    jmp l   ; (bne)

done:
    inc p
    bne r
    inc p+1
r:  jmp cursor_enable
.endproc

.proc print_charset
    jsr cursor_disable
    ldx #0

l3: txa

    pha
    and #%11111
    bne l5
    jsr line_break
l5: pla
    pha

    jsr _term_put

l4: pla
    tax
    inx
    bne l3

    jsr line_break
    jsr line_break
    jmp cursor_enable
.endproc


    .data

txt_welcome:
    .byte 201
    .res 38,205
    .byte 187

    .byte 186, "        VIC 40x24 char terminal       ", 186

    .byte 200
    .res 38,205
    .byte 188

    .byte 13
    .byte "Charset: Code page 437", 13
    .byte 0

    .align 256

charset:
    .include "charset-4x8.asm"
