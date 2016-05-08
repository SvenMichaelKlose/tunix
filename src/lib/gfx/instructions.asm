.export apply, addx, addy, setzb, setzw, addzb, sbczb, sbczbi, setpattern, setzs, pushz, popz

.importzp d, tmp, tmp2, tmp3, bcp, xpos, ypos
.import syscall_vectors_l, syscall_vectors_h
.import next_bytecode, inc_bcp

.code

; Call system function without argument mapping.
.proc apply
    lda (bcp),y
    tax
    dex
    jsr inc_bcp
    lda syscall_vectors_l,x
    sta mod_call+1
    lda syscall_vectors_h,x
    sta mod_call+2
mod_call:
    jmp $ffff
.endproc

.proc addx
    lda tmp
    clc
    adc xpos
    sta xpos
    rts
.endproc

.proc addy
    lda tmp
    clc
    adc ypos
    sta ypos
    rts
.endproc

.proc setzb
    ldx tmp
    lda tmp2
    sta 0,x
    rts
.endproc

.proc setzw
    ldx tmp
    lda tmp2
    sta 0,x
    lda tmp3
    sta 1,x
    rts
.endproc

.proc addzb
    ldx tmp2
    ldy tmp3
    lda 0,x
    clc
    adc 0,y
    ldx tmp
    sta 0,x
    rts
.endproc

.proc sbczb
    ldx tmp2
    ldy tmp3
    lda 0,x
    sec
    sbc 0,y
    ldx tmp
    sta 0,x
    rts
.endproc

.proc sbczbi
    ldx tmp
    lda 0,x
    sec
    sbc tmp2
    sta 0,x
    rts
.endproc

.proc setpattern
    rts
.endproc

.proc setzs
    sty d+1
l:  lda (bcp),y
    sta (d),y
    jsr inc_bcp
    inc d
    bne n
    inc d+1
n:  dec tmp
    bne l
    rts
.endproc

.proc pushz
    pla
    pla
    ldx tmp
    ldy tmp2
l:  lda 0,x
    inx
    pha
    dey
    bne l
    jmp next_bytecode
.endproc

.proc popz
    pla
    pla
    ldy tmp2
    tya
    clc
    adc tmp
    tax
l:  pla
    dex
    sta 0,x
    dey
    bne l
    jmp next_bytecode
.endproc
