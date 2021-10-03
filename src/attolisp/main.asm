.import _term_init, _term_put, _term_puts, _get_key

    .zeropage

heap:   .res 3
p:      .res 3
r:      .res 3
a0:     .res 3
a1:     .res 3
a2:     .res 3

    .code

.proc _main
    jsr _term_init

    lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

    jmp loop
.endproc

.proc loop
l:  jsr _get_key
    jsr _term_put
    jmp l
.endproc

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

.proc err_not_a_cons
    lda #<txt_err_not_a_cons
    ldx #>txt_err_not_a_cons
    jsr _term_puts
    jmp loop
.endproc

    .data

txt_welcome:
    .byte "AttoLisp v0.1", 13
    .byte "Copyright (c) 2021 Sven Michael Klose", 13
    .byte 13
    .byte "UltiMem found: 1MB RAM, 8MB ROM", 13
    .byte "No environment image found!", 13
    .byte "Reading 'env.lisp'...", 13
    .byte "Dumping to 'tmp.img'...", 13
    .byte "GCing 'tmp.img...", 13
    .byte "  Reading image...", 13
    .byte "  Tracing objects...", 13
    .byte "  Relocating objects...", 13
    .byte "  Copying objects...", 13
    .byte "Loading image 'env.lisp'...", 13
    .byte 13
    .byte "Welcome!", 13
    .byte "* "
    .byte 0

txt_err_not_a_cons:
    .byte "Object is not a cons: ", 0

