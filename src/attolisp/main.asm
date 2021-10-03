    .zeropage

heap:   .res 3
p:      .res 3
r:      .res 3
a0:     .res 3
a1:     .res 3
a2:     .res 3

    .code

; X: Size
.proc alloc_heap
    lda heap+1
    cmp #7          ; Last page of heap bank?
    beq check       ; Check low pointer...

    ; Save pointer to object.
l:  lda heap
    sta p
    lda heap+1
    sta p+1
    lda heap+2
    sta p+2

    ; Step to next free space.
    txa
    clc
    adc heap
    sta heap
    lda heap+1
    adc #0
    sta heap+1

    rts

check:
    txa
    clc
    adc heap
    bcc l

    ; Switch to next free bank.
    inc heap+2
    lda heap+2
    sta $9ffc

    ; Reset heap pointer to start of bank.
    lda #$00
    sta heap
    lda #$60
    sta heap+1

    bne l       ; (jmp)
.endproc

.proc alloc_cons
    lda #6
    jsr alloc_heap

    ldy #0
    lda a0
    sta (p),y
    iny
    lda a0+1
    sta (p),y
    iny
    lda a0+2
    sta (p),y
    iny
    lda a1
    sta (p),y
    iny
    lda a1+1
    sta (p),y
    iny
    lda a1+2
    sta (p),y

    rts
.endproc

.proc car
    lda a0+3
    sta $9ffc

    ldy #0
.endproc

.proc cdx
    lda (a0),y
    stx r
    and #128
    bne err_not_a_cons
    iny
    lda (a0),y
    sta r+1
    iny
    lda (a0),y
    sta r+2

    rts
.endproc

.proc cdr
    lda a0+3
    sta $9ffc

    ldy #3
    bne cdx
.endproc

.proc rplaca
    lda a1+3
    sta $9ffc

    ldy #0
.endproc

.proc rplacx
    lda (a1),y
    and #128
    bne err_not_a_cons

    lda a0
    sta (a1),y
    iny
    lda a0+1
    sta (a1),y
    sta r+1
    iny
    lda a0+2
    sta (a1),y
    sta r+2

    rts
.endproc

.proc rplacd
    lda a1+3
    sta $9ffc

    ldy #3
.endproc

.proc error_not_a_cons
.endproc
