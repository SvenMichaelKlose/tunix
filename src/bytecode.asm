; Call system function without argument mapping.
apply:
    lda (sp),y
    tax
    dex
    jsr inc_sp
    lda syscall_vectors_l,x
    sta @(+ 1 +mod_call)
    lda syscall_vectors_h,x
    sta @(+ 2 +mod_call)
mod_call:
    jmp $ffff

addx:
    lda tmp
    clc
    adc xpos
    sta xpos
    rts

addy:
    lda tmp
    clc
    adc ypos
    sta ypos
    rts

setzb:
    ldx tmp
    lda tmp2
    sta 0,x
    rts

setzw:
    ldx tmp
    lda tmp2
    sta 0,x
    lda tmp3
    sta 1,x
    rts

addzb:
    ldx tmp2
    ldy tmp3
    lda 0,x
    clc
    adc 0,y
    ldx tmp
    sta 0,x
    rts

sbczb:
    ldx tmp2
    ldy tmp3
    lda 0,x
    sec
    sbc 0,y
    ldx tmp
    sta 0,x
    rts

sbczbi:
    ldx tmp
    lda 0,x
    sec
    sbc tmp2
    sta 0,x
    rts

setpattern:
    rts
