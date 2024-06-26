.include "../6502/convenience.mac"

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

.macro vfilehead
.endmacro

.macro vfiletail
.endmacro

.macro vsegcode
    .code
.endmacro

.macro vsegdata
    .data
.endmacro

.macro vdatab v
    .byte v
.endmacro

.macro vdataw v
    .word v
.endmacro

.macro vbssb
    .res 1
.endmacro

.macro vdefglobal name
name:
.endmacro

.macro vdeflocal n
@n:
.endmacro

.macro vimport name
.import name
.endmacro

.macro vexport name
.export name
.endmacro

.macro vswap
    mvb tmp1, rega
    mvb tmp2, regb
    mvb rega, tmp2
    mvb regb, tmp1
.endmacro

.macro vldaci v
    stwi rega, v
.endmacro

.macro vldbci v
    stwi regb, v
.endmacro

.macro vldbamc
    ldy #0
    lda (regb),y
    sta regal
.endmacro

.macro vldbamuc
    ldx #0
    ldy #0
    lda (regb),y
    sta regal
    bpl :+
    dex
:   stx regah
.endmacro

.macro vldbami
    ldamc
    iny
    lda (regb),y
    sta regah
.endmacro

.macro vstac p
    mvb p, regal
.endmacro

.macro vstai p
    mvw p, rega
.endmacro

; A += sp
.macro vaddsp
    tsx
    txa
    clc
    adc regal
    sta regal
    lda #1
    adc regah
    sta regah
.endmacro

; A += B
.macro vadda
    lda regal
    clc
    adc regbl
    sta regal
    lda regah
    adc regbl
    sta regah
.endmacro

; B += A
.macro vaddb
    lda regbl
    clc
    adc regal
    sta regbl
    lda regbh
    adc regal
    sta regbh
.endmacro

; A -= B
.macro vsub
    lda regal
    sec
    sbc regbl
    sta regal
    lda regah
    sec regbl
    sta regah
.endmacro

.macro vinca
    inc regal
    bne :+
    inc regah
:
.endmacro

.macro vgetc
    ldy #0
    sta regah
    lda (regb),y
    lda regal
.endmacro

.macro vgetuc
    ldy #0
    lda (regb),y
    sta regal
    bpl :+
    iny
:   sty regah
.endmacro

.macro vgeti
    ldy #0
    lda (regb),y
    sta regal
    iny
    lda (regb),y
    sta regah
.endmacro

.macro vputc
    ldy #0
    lda regal
    sta (regb),y
.endmacro

.macro vputi
    ldy #0
    lda regal
    sta (regb),y
    iny
    lda regah
    sta (regb),y
.endmacro

.macro vdeca
    dec regal
    lda regal
    cmp #255
    bne :+
    dec regah
:
.endmacro

.macro vdecb
    dec regbl
    lda regbl
    cmp #255
    bne :+
    dec regbh
:
.endmacro

.macro vsignext
    ldy #0
    lda regal
    bpl :+
    dey
:   sta regah
.endmacro

.macro vpusha
    lda regal
    pha
    lda regah
    pha
.endmacro

.macro vpushb
    lda regbl
    pha
    lda regbh
    pha
.endmacro

.macro vpopa
    pla
    sta regah
    pla
    sta regal
.endmacro

.macro vpopb
    pla
    sta regbh
    pla
    sta regbl
.endmacro

; Swap primary with top of stack.
.macro vswapstack
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

.macro vincs1
    tsx
    inx
    txs
.endmacro

.macro vincs2
    tsx
    inx
    inx
    txs
.endmacro

.macro vdecs1
    tsx
    dex
    txs
.endmacro

.macro vdecs2
    tsx
    dex
    dex
    txs
.endmacro

.macro vsphl ; TODO
.endmacro

.macro vcall global
    jsr global
.endmacro

.macro vcallptr
    jsr __callptr   ; jmp (rega)
.endmacro

.macro vret
    rts
.endmacro

.macro vjump local
    jump @local
.endmacro

.macro vjumpnz local
    lda regal
    ora regah
    beq :+
    jump @local
:
.endmacro

.macro vjumpz local
    lda regal
    ora regah
    bne :+
    jump @local
:
.endmacro

.macro vjumpcase ; TODO
.endmacro

; A boolean is a boolean is a boolean.
.macro vbool
    ; Do it any way.
    lda regal
    ora regal
    sta regal
.endmacro

.macro vasl
    ldy regbl
:   asl regal
    ror regah
    dey
    bne :-
.endmacro

.macro vlsr
    ldy regbl
:   lsr regah
    ror regal
    dey
    bne :-
.endmacro

.macro vasr
    ldy regbl
    ldx regah
:   cpx #128
    ror regah
    ror regal
    dey
    bne :-
.endmacro

.macro vcompa
    lda #$ff
    eor regal
    sta regal
    lda #$ff
    eor regah
    sta regah
.endmacro

.macro vcompb
    lda #$ff
    eor regbl
    sta regbl
    lda #$ff
    eor regbh
    sta regbh
.endmacro

.macro vlneg
    ldy #$ff
    lda regal
    ora regah
    beq :+
    iny
:   sty regal
    sty regah
.endmacro

.macro vneg
    compa
    inca
.endmacro

.macro mul  ; TODO
.endmacro
.macro div  ; TODO
.endmacro

.macro vumul
    jsr _umul_
.endmacro

.macro vudiv
    jsr _udiv_
.endmacro

.macro vand
    lda regal
    and regbl
    sta regal
    lda regah
    and regbh
    sta regah
.endmacro

.macro vor
    lda regal
    ora regbl
    sta regal
    lda regah
    ora regbh
    sta regah
.endmacro

.macro vxor
    lda regal
    eor regbl
    sta regal
    lda regah
    eor regbh
    sta regah
.endmacro

.macro veq
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

.macro vneq
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

.macro vlt
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

.macro vlte
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

.macro vgt  ; TODO
.endmacro
.macro vgte  ; TODO
.endmacro
.macro vult  ; TODO
.endmacro
.macro vulte  ; TODO
.endmacro
.macro vugt  ; TODO
.endmacro
.macro vugte  ; TODO
.endmacro

.macro vsrcline str
.endmacro
