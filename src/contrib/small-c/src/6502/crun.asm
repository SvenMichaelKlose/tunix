.export cguhcar

    .zeropage

hl:
l:  .res 1
h:  .res 1

de:
e:  .res 1
d:  .res 1

    .code

; Load char at primary into primary.
cguchar:
    ldy #0
    lda (hl),y
    sta l
    sty h
    rts

; https://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
.proc _umul_
    lda	#$00
    sta	product+2
    sta	product+3
    ldx	#16
shift_r:
    lsr	multiplier+1
    ror	multiplier
    bcc	rotate_r
    ; Get upper half of product and
    ; add multiplicand.
    lda	product+2
    clc
    adc	multiplicand
    sta	product+2
    lda	product+3
    adc	multiplicand+1
rotate_r:
    ror	; partial product
    sta	product+3
    ror	product+2
    ror	product+1
    ror	product
    dex
    bne	shift_r
    rts
.endproc

; https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
.proc _udiv_
    lda #0
    sta remainder
    sta remainder+1
    ldx #16

divloop:
    ; dividend lb & hb*2, msb -> Carry
    asl dividend
    rol dividend+1
    ; remainder lb & hb * 2 + msb
    ; from carry
    rol remainder
    rol remainder+1
    lda remainder
    sec
    ; substract divisor to see if it
    ; fits in
    sbc divisor
    ;lb result -> Y, for we may need
    ; it later
    tay
    lda remainder+1
    sbc divisor+1
    ; if carry=0 then divisor didn't
    ; fit in yet
    bcc skip

    ; else save substraction result
    ; as new remainder,
    sta remainder+1
    sty remainder
    ; and INCrement result cause
    ; divisor fit in 1 times
    inc result

skip:
    dex
    bne divloop
    rts
.endproc
