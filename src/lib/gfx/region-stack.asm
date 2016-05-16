.export init_region_stack, push_region, pop_region, add_region, sub_region

.importzp xpos, ypos, rxl, ryt, region_sp

.code

.proc init_region_stack
    lda #<region_stack
    sta region_sp
    lda #>region_stack
    sta region_sp+1
    rts
.endproc

.proc push_region
    ldx #3
    ldy #3
l:  lda rxl,x
    sta (region_sp),y
    dex
    dey
    bpl l

    lda region_sp
    clc
    adc #4
    sta region_sp
    bcc n
    inc region_sp+1
n:

    rts
.endproc

.proc pop_region
    lda region_sp
    sec
    sbc #4
    sta region_sp
    bcs n
    dec region_sp+1
n:

    ldx #3
    ldy #3
l:  lda (region_sp),y
    sta rxl,x
    dex
    dey
    bpl l

    rts
.endproc

.proc add_region
    lda rxl
    clc
    adc xpos
    sta xpos

    lda ryt
    clc
    adc ypos
    sta ypos

    rts
.endproc

.proc sub_region
    lda xpos
    sec
    sbc rxl
    sta xpos

    lda ypos
    sec
    sbc ryt
    sta ypos

    rts
.endproc

.bss

region_stack:
    .res 256, 0
