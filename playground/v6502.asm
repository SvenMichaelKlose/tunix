; Sketch of a virtual 6502 machine

    .zeropage

accu:   .byte ?
flags:  .byte ?
x:      .byte ?
y:      .byte ?
sp:     .byte ?

op:     .word ?

    .data

zeropage:   .fill 256
wxlat:      .fill 256
rxlat:      .fill 256
itab_l:     .fill 256
itab_h:     .fill 256
modes:      .fill 256

fetch:
vpc:ldx $ffff
    inc pc
    bcc +l
    inc pc+1
l:  rts

pc = vpc + 1

exec:
    ldy #0
    jsr fetch
    lda itab_l,x
    sta l1+1
    lda itab_h,x
    sta l1+2
    lda modes,x
    lsr
    bcc l1
    jsr fetch
    stx op
    lsr
    bcc l1
    jsr fetch
    stx op+1
l1: jmp ($1234)

i_ldai:
    jsr getflags
    lda op
setaflags:
    sta accu
setflags:
    php
    pla
    sta flags
    jmp exec

.proc i_ldaz
    ldx op
    jsr getflags
    lda zeropage,x
    bcc setaflags
.endproc

.proc i_ldaa
    ldx op+1
    lda rxlat
    bne dorxlat
    jsr getflags
    lda (op),y
    bcc setaflags
.endproc

.proc i_ldaax
    ldy reg_x
    bcc i_ldaa
.endproc

.proc i_ldaay
    ldy reg_x
    lda (op),y
    bcc i_ldaa
.endproc

.proc i_ldaix
    lda op
    clc
    adc x
    tax
    lda zeropage,x
    sta ptr
    inx
    lda zeropage,x
    sta ptr+1
    tax
    jsr getflags
    lda (ptr),y
    jmp setaflags
.endproc

.proc i_ldaiy
    ldx op
    lda zeropage,x
    sta ptr
    inx
    lda zeropage,x
    sta ptr+1
    jsr getflags
    lda (ptr),y
    bcc setaflags
.endproc

.proc i_staz
    ldx op
    lda accu
    sta zeropage,x
    jmp exec
.endproc

.proc i_staa
    ldx op+1
    lda wxlat,x
    bne dowxlat
    lda accu
    sta (op),y
    jmp exec
.endproc

.proc i_cmpz
    lda flags
    pha
    ldx op
    lda accu
    plp
    cmp zeropage,x
    jmp setflags
.endproc

.proc i_cmpa
    lda flags
    pha
    lda accu
    plp
    cmp (op),y
    jmp setflags
.endproc

.proc i_pha
    lda accu
    ldx sp
    sta stackbase,x
    dec sp
    jmp exec
.endproc

.proc i_pla
    inc sp
    ldx sp
    lda stackbase,x
    bcc setaflags
.endproc

.proc i_php
    lda flags
    ldx sp
    sta stackbase,x
    dec sp
    jmp exec
.endproc

.proc i_plp
    inc sp
    ldx sp
    lda stackbase,x
    sta flags
    jmp exec
.endproc

.proc getflags
    lda flags
    pha
    plp
    rts
.endproc

.proc i_tax
    jsr getflags
    lda accu
    sta x
    jmp setflags
.endproc

.proc i_txa
    jsr getflags
    lda x
    sta accu
    jmp setflags
.endproc

.proc i_tay
    jsr getflags
    lda accu
    sta y
    jmp setflags
.endproc

.proc i_tya
    jsr getflags
    lda y
    sta accu
    jmp setflags
.endproc

.proc i_tsx
    jsr getflags
    lda sp
    sta x
    jmp setflags
.endproc

.proc i_txs
    jsr getflags
    lda x
    sta sp
    jmp setflags
.endproc

.proc i_rts
    ldx sp
    inc sp
    inc sp
    lda stackbase+1,x
    sta pc
    dex
    lda stackbase,x
    sta pc+1
    jmp exec
.endproc
