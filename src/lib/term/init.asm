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
    lda #>charset
    sta font+1

    lda #0
    sta xpos
    sta ypos

    lda #<txt_welcome
    sta p
    lda #>txt_welcome
    sta p+1
    jsr putstring_fixed

l:
    jmp l
    rts
.endproc

.proc putstring_fixed
    ldy #0
    lda (p),y
    beq done

    cmp #13
    bne n

    lda #0
    sta xpos
    lda ypos
    clc
    adc #8
    sta ypos
    jmp next

n:  jsr putchar_fixed

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
    .byte "VI clone for INGLE", 13
    .byte "By Sven Michael Klose", 13
    .byte "   <pixel@hugbox.org>", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "Whatever happens this line is full.12345", 13
    .byte "x", 13
    .byte 0

    .align 256

charset:
    .include "charset-4x8.asm"
