; UNDER ACTIVE CONSTRUCTION!

    .zeropage

; IR machine registers
rega:
regal:  .res 1
regah:  .res 1
regb:
regbl:  .res 1
regbh:  .res 1

tmp1:   .res 2
tmp2:   .res 2

.macro seg_code
    .code
.endmacro

.macro seg_data
    .data
.endmacro

.macro datab x
    .byte x
.endmacro

.macro dataw x
    .word x
.endmacro

.macro bssb
    .res 1
.endmacro

.macro defglobal name
name:
.endmacro

.macro deflocal n
@n:
.endmacro

#define IR_LOCAL 8
#define IR_GLOBAL 9

.macro import name
.import name
.endmacro

.macro export name
.export name
.endmacro

.macro swap
    mvb tmp1, rega
    mvb tmp2, regb
    mvb rega, tmp2
    mvb regb, tmp1
.endmacro

.macro ldaci v
    stwi rega, v
.endmacro

.macro ldbci v
    stwi regb, v
.endmacro

.macro ldbamc
    ldy #0
    lda (regb),y
    sta regal
.endmacro

.macro ldbamuc
    ldx #0
    ldy #0
    lda (regb),y
    sta regal
    bpl :+
    dex
:   stx regah
.endmacro

.macro ldbami
    ldamc
    iny
    lda (regb),y
    sta regah
.endmacro

.macro stac p
    mvb p, regal
.endmacrp

.macro stai p
    mvw p, rega
.endmacrp

; A += sp
.macro addsp
    tsx
    txa
    clc
    adc regal
    sta regal
    lda #1
    adc regah
    sta regah
:
.endmacro

; A += B
.macro adda
    lda regal
    clc
    adc regbl
    sta regal
    lda regah
    adc regbl
    sta regah
.endmacro

; A -= B
.macro sub
    lda regal
    sec
    sbc regbl
    sta regal
    lda regah
    sec regbl
    sta regah
.endmacro

.macro inca
    inc regal
    bne :+
    inc regah
:
.endm

.macro getchar
    ldy #0
    sta regah
    lda (regb),y
    lda regal
.endm

.macro getuchar
    ldy #0
    lda (regb),y
    sta regal
    bpl :+
    iny
:   sty regah
.endm

.macro getint
    ldy #0
    lda (regb),y
    sta regal
    iny
    lda (regb),y
    sta regah
.endm

.macro putchar
    ldy #0
    lda regal
    sta (regb),y
.endm

.macro putint
    ldy #0
    lda regal
    sta (regb),y
    iny
    lda regah
    sta (regb),y
.endm

.macro deca
    dec regal
    lda regal
    cmp #255
    bne :+
    dec regah
:
.endm

.macro decb
    dec regbl
    lda regbl
    cmp #255
    bne :+
    dec regbh
:
.endm

.macro signext
    ldy #0
    lda regal
    bpl :+
    dey
:   sta regah
.endm

.macro pusha
    lda regal
    pha
    lda regah
    pha
.endmacro

.macro pushb
    lda regbl
    pha
    lda regbh
    pha
.endmacro

.macro popa
    pla
    sta regah
    pla
    sta regal
.endmacro

.macro popb
    pla
    sta regbh
    pla
    sta regbl
.endmacro

; Swap primary with top of stack.
.macro swapstack
    pla
    sta tmp1
    pla
    sta tmp1+1
    lda regal
    pha
    lda regah
    pha
    lda tmp1
    sta regal
    lda tmp1+1
    sta regah
.endmacro

.macro incs1
    tsx
    inx
    txs
.endmacro

.macro incs2
    tsx
    inx
    inx
    txs
.endmacro

.macro decs1
    tsx
    dex
    txs
.endmacro

.macro decs2
    tsx
    dex
    dex
    txs
.endmacro

#define IR_SPHL 39

.macro call global
    jsr global
.endmacro

.macro callptr
    jsr __callptr   ; jmp (rega)
.endmacro

.macro ret
    rts
.endmacro

.macro jmp local
    jmp @local
.endmacro

.macro jmpnz local
    lda regal
    ora regah
    beq :+
    jmp @local
:
.endmacro

.macro jmpz local
    lda regal
    ora regah
    bne :+
    jmp @local
:
.endmacro

#define IR_JMPCASE 46

; A boolean is a boolean is a boolean.
.macro bool
.endmacro

.macro asl
    ldy regbl
:   asl regal
    ror regah
    dey
    bne :-
.endm

.macro lsr
    ldy regbl
:   lsr regah
    ror regal
    dey
    bne :-
.endm

.macro asr
    ldy regbl
    ldx regah
:   cpx #128
    ror regah
    ror regal
    dey
    bne :-
.endm

.macro compa
    lda #$ff
    eor regal
    sta regal
    lda #$ff
    eor regah
    sta regah
.endmacro

.macro compb
    lda #$ff
    eor regbl
    sta regbl
    lda #$ff
    eor regbh
    sta regbh
.endmacro

.macro lneg
    ldy #$ff
    lda regal
    ora regah
    beq :+
    iny
:   sty regal
    sty regah
.endmacro

.macro neg
    compa
    inca
.endmacro

#define IR_MUL 57
#define IR_DIV 58

.macro umul
    jsr _umul_
.endmacro

.macro udiv
    jsr _udiv_
.endmacro

.macro and
    lda regal
    and regbl
    sta regal
    lda regah
    and regbh
    sta regah
.endmacro

.macro or
    lda regal
    ora regbl
    sta regal
    lda regah
    ora regbh
    sta regah
.endmacro

.macro xor
    lda regal
    eor regbl
    sta regal
    lda regah
    eor regbh
    sta regah
.endmacro

.macro eq
    ldx #0
    lda regal
    stx regal
    cmp regbl
    bne :+
    lda regah
    cmp regbh
    beq :+
:   inx
:   stx regal
.endmacro

.macro neq
    ldx #0
    lda regal
    stx regal
    cmp regbl
    beq :+
    inx
    lda regah
    cmp regbh
    bne :+
:   inx
:   stx regal
.endmacro

.macro lt
    ldx #0
    lda regah
    stx regah
    cmp regbh
    bcs :+
    inx
    lda regah
    cmp regbh
    bcc :+
:   inx
:   stx regal
.endmacro

.macro lte
    ldx #0
    lda regah
    stx regah
    cmp regbh
    beq :+
    bcs :++
:   lda regah
    cmp regbh
    beq :++
    bcc :++
:   inx
:   stx regal
.endmacro

#define IR_GT 69
#define IR_GTE 70
#define IR_ULT 71
#define IR_ULTE 72
#define IR_UGT 73
#define IR_UGTE 74

.macro srcline str
.endmacro
