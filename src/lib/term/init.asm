.export _term_init

.import gfx_init
.import clear_screen
.import putchar_fixed

.importzp s, d, c, font, xpos, ypos

    .zeropage

tmp:    .res 1
tmp2:   .res 1
p:      .res 2

    .code

.proc _term_init
    jsr clear_screen
    jsr gfx_init

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

    jsr print_charset
l:
    jmp l
    rts
.endproc

.proc print_charset
    ldx #0
l3: txa
    pha
    jsr putchar_fixed
    jsr cursor_step

l4: pla
    tax
;    cmp #%1111
;    bne l5
;    jsr line_break
l5: inx
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

    .data

txt_welcome:
    .byte "VIC 40x24 char terminal", 13
    .byte 13
    .byte "Charset:", 13
    .byte 13
    .byte 0

    .align 256

charset:
    .include "charset-4x8.asm"
