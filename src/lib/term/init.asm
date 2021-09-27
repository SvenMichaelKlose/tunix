.export _term_init, _term_put

.import init_bitmap_mode, gfx_init
.import clear_screen
.import putchar_fixed

.importzp s, d, c, font, xpos, ypos, scrbase

    .zeropage

tmp:        .res 1
tmp2:       .res 1
p:          .res 2
cursor_x:   .res 1
cursor_y:   .res 1

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

    lda #0
    sta xpos
    sta ypos

    lda #<txt_welcome
    sta p
    lda #>txt_welcome
    sta p+1
    jsr putstring_fixed

    jmp print_charset
.endproc

.proc print_charset
    ldx #0

l3: txa

    pha
    and #%11111
    bne l5
    jsr line_break
l5: pla
    pha

    jsr putchar_fixed
    jsr cursor_step

l4: pla
    tax
    inx
    bne l3
    rts
.endproc

.proc cursor_step
    lda xpos
    clc
    adc #4
    sta xpos

    cmp #160
    bne n

    lda #0
    sta xpos
    jmp cursor_down

n:  rts
.endproc

.proc cursor_down
    lda ypos
    clc
    adc #8
    sta ypos
    rts
.endproc

.proc line_break
    lda #0
    sta xpos
    jmp cursor_down
.endproc

.proc putstring_fixed
    ldy #0
    lda (p),y
    beq done

    cmp #13
    bne n
    jsr line_break
    jmp next

n:  jsr putchar_fixed
    jsr cursor_step

next:
    inc p
    bne putstring_fixed
    inc p+1
    jmp putstring_fixed

done:
    inc p
    bne r
    inc p+1
r:  rts
.endproc

.proc _term_put
    rts
.endproc

    .data

txt_welcome:
    .byte 201
    .res 38,205
    .byte 187

    .byte 186, "        VIC 40x24 char terminal       ", 186
    ;.byte 186, "VIC 40x24 char terminal               ", 186

    .byte 200
    .res 38,205
    .byte 188

    .byte 13
    .byte "Charset: Code page 437", 13
    .byte 0

    .align 256

charset:
    .include "charset-4x8.asm"
